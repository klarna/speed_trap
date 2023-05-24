%%%=============================================================================
%%% @doc speed_trap a simple yet effective rate_limiter for Erlang
%%%
%%% speed_trap uses the token bucket algorithm and implements this algorithm
%%% by making use of atomics to represent a bucket.
%%%
%%% Granularity of buckets is provided by allowing new buckets (see: {@link speed_trap:new/2})
%%% to be created using any arbitrary {@link speed_trap:id()}.
%%%
%%% An example of such id could be:
%%% `{<<localhost:8080/path/to/resource">>, <<"POST">>}'
%%% with a generic id being `<<"POST">>'
%%%
%%% Generic buckets can subsequently be setup (see: {@link speed_trap:new/2})
%%% using a more generic {@link speed_trap:id()} such as just `<<"username">>'.
%%%
%%% Each time a request is made, one simpy checks if allowed by
%%% using {@link speed_trap:try_pass/1}.
%%%
%%% In case a bucket needs modification, either increase or decrease the size and interval,
%%% one can use the {@link speed_trap:modify/2} in order to adjust the limits.
%%%
%%% @copyright 2023 Klarna Bank AB
%%% @end
%%%=============================================================================
-module(speed_trap).

-export([new/2, delete/1, try_pass/1, try_pass_all/1, modify/2, options/1, all/0, block/1,
         unblock/1]).

-type id() :: term().
-type bucket_size() :: non_neg_integer().
-type refill_interval() :: pos_integer().
-type refill_count() :: pos_integer().
-type override() :: none | not_enforced | blocked.
-type already_exists() :: already_exists.
-type blocked() :: blocked.
-type no_such_speed_trap() :: no_such_speed_trap.
-type too_many_requests() :: too_many_requests.
-type rate_limit_not_enforced() :: rate_limit_not_enforced.
-type try_pass_success() :: non_neg_integer() | rate_limit_not_enforced().
-type try_pass_failure() ::
  blocked() | no_such_speed_trap() | too_many_requests() | speed_trap_options:bad_options().
-type try_pass_result() :: {ok, try_pass_success()} | {error, try_pass_failure()}.
-type try_pass_all_result() :: ok | {error, id(), try_pass_failure()}.
-type options() ::
  #{bucket_size := bucket_size(),
    refill_interval := refill_interval(),
    refill_count := refill_count(),
    delete_when_full := boolean(),
    override := override(),
    _ => any()}.
-type stored_options() ::
  #{bucket_size := bucket_size(),
    refill_interval := refill_interval(),
    refill_count := refill_count(),
    delete_when_full := boolean(),
    override := override(),
    _ => any()}.
-type modify_options() ::
  #{bucket_size => bucket_size(),
    refill_interval => refill_interval(),
    refill_count => refill_count(),
    delete_when_full => boolean(),
    override => override(),
    _ => any()}.

-export_type([id/0, bucket_size/0, refill_interval/0, refill_count/0, options/0, stored_options/0,
              modify_options/0, already_exists/0, no_such_speed_trap/0, too_many_requests/0,
              try_pass_success/0, blocked/0]).

%% @doc Setup a new TokenBucket.
%% This is where a rate_limiter is setup for any arbitrary identifier.
-spec new(id(), options()) -> ok | {error, already_exists() | speed_trap_options:bad_options()}.
new(Id, Options) ->
  case speed_trap_options:validate(Options, _MandatoryOptions = true) of
    ok ->
      case maps:is_key(override, Options) of
        true ->
          speed_trap_token_bucket:new(Id, Options);
        false ->
          speed_trap_token_bucket:new(Id, Options#{override => none})
      end;
    Errors ->
      Errors
  end.

%% @doc Removes a TokenBucket and hence removes a rate limiter.
-spec delete(id()) -> ok | {error, no_such_speed_trap()}.
delete(Id) ->
  speed_trap_token_bucket:delete(Id).

-spec all() -> [{id(), stored_options()}].
all() ->
  speed_trap_token_bucket:active_buckets().

%% @doc Try grabbing a token from a TokenBucket.
%% As long as this function is ok, the request is not rate limited.
-spec try_pass(id()) -> try_pass_result().
try_pass(Id) ->
  AllowCreationFromTemplate = true,
  do_try_pass(Id, AllowCreationFromTemplate).

-spec try_pass_all([id()]) -> try_pass_all_result().
try_pass_all(Ids) ->
  try_pass_all(Ids, []).

%% @doc Modify an existing TokenBucket and decrease/increase its size and/or refill interval.
-spec modify(id(), modify_options()) ->
              ok | {error, no_such_speed_trap() | speed_trap_options:bad_options()}.
modify(Id, Options) ->
  case speed_trap_options:validate(Options, _MandatoryOptions = false) of
    ok ->
      speed_trap_token_bucket:modify(Id, Options);
    Errors ->
      Errors
  end.

-spec options(id()) -> {ok, stored_options()} | {error, no_such_speed_trap()}.
options(Id) ->
  speed_trap_token_bucket:options(Id).

-spec block(id()) -> ok | {error, speed_trap_options:bad_options() | no_such_speed_trap()}.
block(Id) ->
  modify(Id, #{override => blocked}).

-spec unblock(id()) -> ok | {error, no_such_speed_trap()}.
unblock(Id) ->
  modify(Id, #{override => none}).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
-spec do_try_pass(id(), boolean()) -> try_pass_result().
do_try_pass(Id, AllowCreationFromTemplate) ->
  case speed_trap_token_bucket:get_token(Id) of
    {error, no_such_speed_trap} when AllowCreationFromTemplate ->
      try_pass_from_template(Id);
    Res ->
      Res
  end.

-spec try_pass_from_template(id()) -> try_pass_result().
try_pass_from_template(Id) ->
  case speed_trap_template:options_from_id(Id) of
    {ok, TemplateId, Options} ->
      case new(Id, Options#{template_id => TemplateId}) of
        ok ->
          do_try_pass(Id, false);
        {error, already_exists} ->
          %% A concurrent request has created a speed_trap in the meantime
          %% Since it was just created, we can simply call do_try_pass/2 now
          do_try_pass(Id, false);
        {error, {bad_options, _}} = Error ->
          Error
      end;
    not_found ->
      {error, no_such_speed_trap}
  end.

-spec try_pass_all([id()], [id()]) -> try_pass_all_result().
try_pass_all([], _) ->
  ok;
try_pass_all([Id | RestIds], PassedIds) ->
  case try_pass(Id) of
    {ok, _} ->
      try_pass_all(RestIds, [Id | PassedIds]);
    {error, Reason} ->
      lists:foreach(fun speed_trap_token_bucket:return_token/1, PassedIds),
      {error, Id, Reason}
  end.
