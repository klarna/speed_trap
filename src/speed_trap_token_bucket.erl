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

-export([start_link/0, bucket/1, new/2, delete/1, modify/2, get_token/1, return_token/1, options/1,
         active_buckets/0]).
%% Timer callback
-export([add_token/4]).

-behaviour(gen_server).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2]).

-type token_bucket() :: atomics:atomics_ref().

-export_type([token_bucket/0]).

-define(SERVER, ?MODULE).
-define(ETS_TABLE, speed_trap).

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

%% @doc Delete a scheduled function by its id.
-spec delete(speed_trap:id()) -> ok | {error, speed_trap:no_such_speed_trap()}.
delete(Id) ->
  gen_server:call(?SERVER, {delete, Id}).

%% @doc Modify the scheduled function already registered under the
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
    {ok, {Options, Blocked, Ctr}} ->
      do_get_token(Options, Blocked, Ctr)
  end.

-spec return_token(speed_trap:id()) -> ok | {error, speed_trap:no_such_speed_trap()}.
return_token(Id) ->
  case bucket(Id) of
    {error, no_such_speed_trap} = E ->
      E;
    {ok, {Options, _Blocked, Ctr}} ->
      #{bucket_size := BucketSize, delete_when_full := DeleteWhenFull} = Options,
      add_token(Ctr, BucketSize, 1, DeleteWhenFull)
  end.

-spec options(speed_trap:id()) ->
               {ok, speed_trap:stored_options()} | {error, speed_trap:no_such_speed_trap()}.
options(Id) ->
  case bucket(Id) of
    {error, no_such_speed_trap} = E ->
      E;
    {ok, {Options, _Blocked, _Ctr}} ->
      {ok, Options}
  end.

-spec bucket(speed_trap:id()) ->
              {ok, {speed_trap:stored_options(), boolean(), token_bucket()}} |
              {error, speed_trap:no_such_speed_trap()}.
bucket(Id) ->
  case ets:lookup(?ETS_TABLE, Id) of
    [] ->
      {error, no_such_speed_trap};
    [{_Id, {Options, Blocked, Bucket}}] ->
      {ok, {Options, Blocked, Bucket}}
  end.

-spec active_buckets() -> [{speed_trap:id(), speed_trap:stored_options()}].
active_buckets() ->
  Ids = gen_server:call(?SERVER, active_ids), %% Timeout?
  lists:filtermap(fun(Id) ->
                     case bucket(Id) of
                       {ok, {Opts, _Blocked, Bucket}} ->
                         {true, {Id, Opts#{tokens => atomics:get(Bucket, 1)}}};
                       {error, no_such_speed_trap} ->
                         false
                     end
                  end,
                  Ids).

%%-----------------------------------------------------------------------------
%% gen_server callbacks
%%-----------------------------------------------------------------------------
-spec init([]) -> {ok, state()}.
init([]) ->
  speed_trap = ets:new(?ETS_TABLE, [set, protected, named_table, {read_concurrency, true}]),
  {ok, #{}}.

-spec handle_call(term(), {pid(), term()}, state()) -> {reply, ok | {error, term()}, state()}.
handle_call({register, Id, #{bucket_size := BucketSize} = Options}, _From, Timers) ->
  case maps:is_key(Id, Timers) of
    true ->
      {reply, {error, already_exists}, Timers};
    false ->
      Ctr = atomics:new(1, [{signed, true}]),
      ok = atomics:put(Ctr, 1, BucketSize),
      Timer = init_trap(Id, Ctr, Options),
      {reply, ok, maps:put(Id, Timer, Timers)}
  end;
handle_call({delete, Id}, _From, Timers) ->
  case maps:take(Id, Timers) of
    {Timer, RemainingTimers} ->
      timer:cancel(Timer),
      true = ets:delete(?ETS_TABLE, Id),
      {reply, ok, RemainingTimers};
    error ->
      {reply, {error, no_such_speed_trap}, Timers}
  end;
handle_call({modify, Id, NewOptions}, _From, Timers) ->
  case maps:find(Id, Timers) of
    {ok, OldTimer} ->
      timer:cancel(OldTimer),
      {ok, {OldOptions, _Blocked, Ctr}} = bucket(Id),
      Options = maps:merge(OldOptions, NewOptions),
      case speed_trap_options:check_bad_combination(Options) of
        ok ->
          Timer = init_trap(Id, Ctr, Options),
          {reply, ok, maps:update(Id, Timer, Timers)};
        {error, _BadOptions} = Error ->
          {reply, Error, Timers}
      end;
    error ->
      {reply, {error, no_such_speed_trap}, Timers}
  end;
handle_call(active_ids, _From, Timers) ->
  Ids = maps:keys(Timers),
  {reply, Ids, Timers};
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
add_token(Ctr, BucketSize, RefillCount, DeleteWhenFull) ->
  case atomics:get(Ctr, 1) of
    N when N =:= BucketSize andalso DeleteWhenFull ->
      Id = ctr_to_id(Ctr),
      case delete(Id) of
        ok ->
          ok;
        {error, no_such_speed_trap} ->
          ok
      end;
    N when N =< 0 ->
      atomics:put(Ctr, 1, RefillCount);
    N when N < BucketSize ->
      %% Do not overflow the bucket
      RefillN = min(RefillCount, BucketSize - N),
      case atomics:add_get(Ctr, 1, RefillN) of
        M when M < RefillCount ->
          %% After a refill the number of tokens in the bucket should be in
          %% the [RefillCount, BucketSize] range. If it is lower than RefillCount
          %% right after the refill it means that a user has consumed some tokens
          %% concurrently between our first get and the add_get the bucket.
          atomics:put(Ctr, 1, RefillCount);
        _M ->
          ok
      end;
    _N ->
      ok % the bucket is already full
  end.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
-spec init_trap(speed_trap:id(), atomics:atomics_ref(), speed_trap:options()) -> timer:tref().
init_trap(Id, Ctr, Options) ->
  #{bucket_size := BucketSize,
    refill_interval := RefillInterval,
    refill_count := RefillCount,
    delete_when_full := DeleteWhenFull} =
    Options,
  case atomics:get(Ctr, 1) of
    N when N > BucketSize ->
      ok = atomics:put(Ctr, 1, BucketSize);
    _ ->
      ok
  end,
  {ok, Timer} =
    timer:apply_interval(RefillInterval,
                         ?MODULE,
                         add_token,
                         [Ctr, BucketSize, RefillCount, DeleteWhenFull]),
  IsBlocked = speed_trap_options:is_blocked(Options),
  true = ets:insert(?ETS_TABLE, {Id, {Options#{blocked => IsBlocked}, IsBlocked, Ctr}}),
  Timer.

ctr_to_id(Ctr) ->
  [Id] =
    ets:select(?ETS_TABLE, ets:fun2ms(fun({Id, {_Options, _Blocked, C}}) when C =:= Ctr -> Id end)),
  Id.

do_get_token(_Options, true = _Blocked, _Ctr) ->
  {error, blocked};
do_get_token(Options, false = _Blocked, Ctr) ->
  case atomics:sub_get(Ctr, 1, 1) of
    N when N >= 0 ->
      {ok, N};
    _ ->
      case speed_trap_options:is_rate_limit_enforced(Options) of
        true ->
          {error, too_many_requests};
        false ->
          {ok, rate_limit_not_enforced}
      end
  end.
