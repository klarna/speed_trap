%%%=============================================================================
%%% @doc speed_trap_token_bucket implementation with atomics.
%%%
%%% The current number of tokens is stored as a mutable atomic
%%% variable. When a token is taken from the bucket, the counter is
%%% decremented. It is allowed for the counter to become negative.
%%%
%%% Adding tokens to the counter requires multiple operations. Since
%%% there is no operation to add with a threshold (contrary to
%%% {@link ets:update_counter/3} for example), the value is first read
%%% and then incremented if necessary. It is possible that the value
%%% will be decremented between the two operations, but it cannot be
%%% incremented (given there's only one updater running at a time), so
%%% there's no risk of adding tokens above the bucket's capacity. On
%%% the other hand, the counter going below 0 is an issue that needs
%%% special handling: whenever the update operation finds a negative
%%% value, it sets the counter to 1. Incrementing it is not safe in
%%% this case, as concurrent processes may try to get tokens from the
%%% bucket, pushing its value to even lower between the read and the
%%% update.
%%%
%%% It's worth mentioning that although counters can underflow, it is
%%% not a practical risk to call {@link get_token/1} 2^63 times in a
%%% single refill interval.
%%%
%%% For simplicity, the actual updates are scheduled via
%%% `timer:apply_interval/4', however an interval timer is linked to
%%% the process creating it. Therefore we need a simple server to own
%%% and manage these timers.
%%%
%%% @copyright 2023 Klarna Bank AB
%%% @end
%%%=============================================================================
-module(speed_trap_token_bucket).

-export([start_link/0, bucket/1, new/2, delete/2, modify/2, get_token/1, return_token/1, options/1,
         active_buckets/0, get_override/1, delete_override/1, delete_overrides/0]).
%% Timer callback
-export([add_token/4]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type token_bucket() :: atomics:atomics_ref().

-export_type([token_bucket/0]).

-define(SERVER, ?MODULE).
-define(ETS_SPEED_TRAPS, speed_traps).
-define(ETS_TEMPLATE_BASED_TRAP_OVERRIDES, speed_trap_template_based_overrides).
-define(BUCKET_IDX, 1).
-define(not_found, not_found).

-type state() :: #{speed_trap:id() => timer:tref()}.

-include_lib("stdlib/include/ms_transform.hrl").

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------
%% @doc Start the scheduler server.
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
  gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Add a new function to the scheduler. The provided MFA will be
%% registered under the given id and applied every refill interval
%% milliseconds.
-spec new(speed_trap:id(), speed_trap:options()) -> ok | {error, speed_trap:already_exists()}.
new(Id, Options) ->
  gen_server:call(?SERVER, {register, Id, Options}).

%% @doc Deletes a scheduled function by its id. The second parameter indicates whether to try
%% deleting a store template based token bucket override by id or not.
-spec delete(speed_trap:id(), boolean()) -> ok | {error, speed_trap:no_such_speed_trap()}.
delete(Id, DeleteOverride) ->
  gen_server:call(?SERVER, {delete, Id, DeleteOverride}).

%% @doc Modifies the scheduled function already registered under the
%% given id.
-spec modify(speed_trap:id(), speed_trap:modify_options()) ->
              ok | {error, speed_trap_options:bad_options() | speed_trap:no_such_speed_trap()}.
modify(Id, Options) ->
  gen_server:call(?SERVER, {modify, Id, Options}).

-spec get_token(speed_trap:id()) ->
                 {ok, speed_trap:try_pass_success()} |
                 {error,
                  speed_trap:no_such_speed_trap() |
                  speed_trap:too_many_requests() |
                  speed_trap:blocked()}.
get_token(Id) ->
  case bucket(Id) of
    {error, no_such_speed_trap} = E ->
      E;
    {ok, {Options, Bucket}} ->
      do_get_token(Options, Bucket)
  end.

-spec return_token(speed_trap:id()) -> ok | {error, speed_trap:no_such_speed_trap()}.
return_token(Id) ->
  case bucket(Id) of
    {error, no_such_speed_trap} = E ->
      E;
    {ok, {_Options, Bucket}} ->
      atomics:add(Bucket, ?BUCKET_IDX, 1)
  end.

-spec options(speed_trap:id()) ->
               {ok, speed_trap:stored_options()} | {error, speed_trap:no_such_speed_trap()}.
options(Id) ->
  case bucket(Id) of
    {error, no_such_speed_trap} = E ->
      E;
    {ok, {Options, _Bucket}} ->
      {ok, Options}
  end.

-spec bucket(speed_trap:id()) ->
              {ok, {speed_trap:stored_options(), token_bucket()}} |
              {error, speed_trap:no_such_speed_trap()}.
bucket(Id) ->
  case ets:lookup(?ETS_SPEED_TRAPS, Id) of
    [] ->
      {error, no_such_speed_trap};
    [{_Id, {Options, Bucket}}] ->
      {ok, {Options, Bucket}}
  end.

-spec active_buckets() -> [{speed_trap:id(), speed_trap:stored_options()}].
active_buckets() ->
  [{Id, Options#{tokens => atomics:get(Bucket, ?BUCKET_IDX)}}
   || {Id, {Options, Bucket}} <- ets:tab2list(?ETS_SPEED_TRAPS)].

%% @doc Gets an override from the overrides store.
-spec get_override(speed_trap:id()) -> {ok, speed_trap:modify_options()} | {error, ?not_found}.
get_override(Id) ->
  case ets:lookup(?ETS_TEMPLATE_BASED_TRAP_OVERRIDES, Id) of
    [] ->
      {error, ?not_found};
    [{_, Options}] ->
      {ok, Options}
  end.

%% @doc Deletes a possibly stored token bucket override from the store.
-spec delete_override(speed_trap:id()) -> ok.
delete_override(Id) ->
  gen_server:call(?SERVER, {delete_override, Id}).

%% @doc Cleans up the overrides store.
-spec delete_overrides() -> ok.
delete_overrides() ->
  gen_server:call(?SERVER, delete_overrides).

%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  ?ETS_SPEED_TRAPS =
    ets:new(?ETS_SPEED_TRAPS, [set, protected, named_table, {read_concurrency, true}]),
  ?ETS_TEMPLATE_BASED_TRAP_OVERRIDES =
    ets:new(?ETS_TEMPLATE_BASED_TRAP_OVERRIDES,
            [set, protected, named_table, {read_concurrency, true}]),
  {ok, #{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok | {error, term()}, state()}.
handle_call({register, Id, Options}, _From, Timers) ->
  case bucket(Id) of
    {ok, _} ->
      {reply, {error, already_exists}, Timers};
    {error, no_such_speed_trap} ->
      Bucket = atomics:new(1, [{signed, true}]),
      NewTimers = apply_options(Id, Options, Bucket, Timers),
      {reply, ok, NewTimers}
  end;
handle_call({delete, Id, DeleteTemplateModification}, _From, Timers) ->
  case DeleteTemplateModification of
    true ->
      true = ets:delete(?ETS_TEMPLATE_BASED_TRAP_OVERRIDES, Id);
    _ ->
      ok
  end,
  case bucket(Id) of
    {ok, _} ->
      RemainingTimers = cancel_timer_if_exists(Id, Timers),
      true = ets:delete(?ETS_SPEED_TRAPS, Id),
      {reply, ok, RemainingTimers};
    {error, _NoSuchSpeedTrap} = Error ->
      {reply, Error, Timers}
  end;
handle_call({modify, Id, NewOptions}, _From, Timers) ->
  case bucket(Id) of
    {ok, {OldOptions, Bucket}} ->
      ApplyOptsFn = fun(Opts) -> apply_options(Id, Opts, Bucket, Timers) end,
      modify_options_and_reply(Id, OldOptions, NewOptions, ApplyOptsFn, Timers);
    {error, _NoSuchSpeedTrap} = Error ->
      case try_get_options_from_templates(Id) of
        {ok, OldOptions} ->
          modify_options_and_reply(Id, OldOptions, NewOptions, undefined, Timers);
        {error, ?not_found} ->
          {reply, Error, Timers}
      end
  end;
handle_call({delete_override, Id}, _From, Timers) ->
  true = ets:delete(?ETS_TEMPLATE_BASED_TRAP_OVERRIDES, Id),
  {reply, ok, Timers};
handle_call(delete_overrides, _From, Timers) ->
  true = ets:delete_all_objects(?ETS_TEMPLATE_BASED_TRAP_OVERRIDES),
  {reply, ok, Timers};
handle_call(_Request, _From, State) ->
  {reply, {error, no_such_call}, State}.

-spec handle_cast(any(), state()) -> {noreply, state()}.
handle_cast(_Request, Timers) ->
  {noreply, Timers}.

-spec handle_info(any(), state()) -> {noreply, state()}.
handle_info(_Request, Timers) ->
  {noreply, Timers}.

%-----------------------------------------------------------------------------
%% Timer callback
%%-----------------------------------------------------------------------------
-spec add_token(token_bucket(), speed_trap:bucket_size(), speed_trap:refill_count(), boolean()) ->
                 ok.
add_token(Bucket, BucketSize, RefillCount, DeleteWhenFull) ->
  case atomics:get(Bucket, ?BUCKET_IDX) of
    N when N >= BucketSize andalso DeleteWhenFull ->
      Id = bucket_to_id(Bucket),
      %% Do not try to delete a template modification
      case delete(Id, false) of
        ok ->
          ok;
        {error, no_such_speed_trap} ->
          ok
      end;
    N when N =< 0 ->
      atomics:put(Bucket, ?BUCKET_IDX, RefillCount);
    N when N < BucketSize ->
      %% Do not overflow the bucket
      RefillN = min(RefillCount, BucketSize - N),
      case atomics:add_get(Bucket, ?BUCKET_IDX, RefillN) of
        M when M < RefillCount ->
          %% After a refill the number of tokens in the bucket should be in
          %% the [RefillCount, BucketSize] range. If it is lower than RefillCount
          %% right after the refill it means that a user has consumed some tokens
          %% concurrently between our first get and the add_get the bucket.
          atomics:put(Bucket, ?BUCKET_IDX, RefillCount);
        _M ->
          ok
      end;
    _N ->
      ok % the bucket is already full
  end.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
-spec apply_options(speed_trap:id(), speed_trap:options(), token_bucket(), state()) -> state().
apply_options(Id, Options, Bucket, State) ->
  NewState = cancel_timer_if_exists(Id, State),
  ets:insert(?ETS_SPEED_TRAPS, {Id, {Options, Bucket}}),
  case Options of
    #{override := blocked} ->
      atomics:put(Bucket, ?BUCKET_IDX, 0),
      NewState;
    #{bucket_size := BucketSize,
      refill_interval := RefillInterval,
      refill_count := RefillCount,
      delete_when_full := DeleteWhenFull} ->
      atomics:put(Bucket, ?BUCKET_IDX, BucketSize),
      {ok, Timer} =
        timer:apply_interval(RefillInterval,
                             ?MODULE,
                             add_token,
                             [Bucket, BucketSize, RefillCount, DeleteWhenFull]),
      State#{Id => Timer}
  end.

-spec cancel_timer_if_exists(speed_trap:id(), state()) -> state().
cancel_timer_if_exists(Id, State) ->
  case maps:take(Id, State) of
    {Timer, NewState} ->
      timer:cancel(Timer),
      NewState;
    error ->
      State
  end.

bucket_to_id(Bucket) ->
  [Id] =
    ets:select(?ETS_SPEED_TRAPS, ets:fun2ms(fun({Id, {_Options, B}}) when B =:= Bucket -> Id end)),
  Id.

do_get_token(#{override := blocked}, _Bucket) ->
  {error, blocked};
do_get_token(#{override := Override}, Bucket) ->
  case atomics:sub_get(Bucket, ?BUCKET_IDX, 1) of
    N when N >= 0 ->
      {ok, N};
    _ when Override =:= not_enforced ->
      {ok, rate_limit_not_enforced};
    _ ->
      {error, too_many_requests}
  end.

-spec update_override(speed_trap:id(), speed_trap:modify_options()) -> true.
update_override(Id, Override) ->
  NewOverride =
    case get_override(Id) of
      {ok, StoredOverride} ->
        maps:merge(StoredOverride, Override);
      {error, ?not_found} ->
        Override
    end,
  true = ets:insert(?ETS_TEMPLATE_BASED_TRAP_OVERRIDES, {Id, NewOverride}).

-spec try_get_options_from_templates(speed_trap:id()) ->
                                      {ok, speed_trap:options()} | {error, ?not_found}.
try_get_options_from_templates(Id) ->
  case speed_trap_template:options_from_id(Id) of
    {ok, TemplateId, TemplateOptions} ->
      Options =
        case get_override(Id) of
          {ok, Override} ->
            maps:merge(TemplateOptions, Override);
          {error, ?not_found} ->
            TemplateOptions
        end,
      {ok, Options#{template_id => TemplateId}};
    ?not_found ->
      {error, ?not_found}
  end.

-spec modify_options_and_reply(speed_trap:id(),
                               speed_trap:options(),
                               speed_trap:modify_options(),
                               fun((speed_trap:options()) -> state()) | undefined,
                               state()) ->
                                {reply, ok, state()} |
                                {reply, {error, speed_trap_options:bad_options()}, state()}.
modify_options_and_reply(Id, OldOptions, NewOptions, ApplyOptsFn, Timers) ->
  Options = maps:merge(OldOptions, NewOptions),
  case speed_trap_options:validate(Options, _Required = false) of
    ok ->
      NewTimers =
        case ApplyOptsFn of
          _ when is_function(ApplyOptsFn) ->
            ApplyOptsFn(Options);
          undefined ->
            Timers
        end,
      case maps:is_key(template_id, OldOptions) of
        true ->
          update_override(Id, NewOptions);
        false ->
          ok
      end,
      {reply, ok, NewTimers};
    {error, _BadOptions} = Error ->
      {reply, Error, Timers}
  end.
