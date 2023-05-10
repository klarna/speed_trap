-module(speed_trap_options).

-export([validate/2, check_bad_combination/1, is_blocked/1, is_rate_limit_enforced/1]).

-type bad_options() :: {bad_options, [tuple(), ...]}.

-export_type([bad_options/0]).

-spec validate(term(), boolean()) -> ok | {error, bad_options()}.
validate(Options, AllRequired) when is_map(Options) ->
  Errors =
    lists:foldl(fun({Key, Mandatory, ValidationFn}, Errs) ->
                   case validate_key(Key, Options, ValidationFn, Mandatory, AllRequired) of
                     ok ->
                       Errs;
                     {error, Reason} ->
                       [Reason | Errs]
                   end
                end,
                [],
                [{bucket_size, true, fun validate_bucket_size/1},
                 {refill_interval, true, fun validate_refill_interval/1},
                 {refill_count, true, fun validate_refill_count/1},
                 {delete_when_full, true, fun validate_delete_when_full/1},
                 {enforce_rate_limit, false, fun validate_enforce_rate_limit/1},
                 {blocked, false, fun validate_blocked/1}]),
  case Errors of
    [] ->
      check_bad_combination(Options);
    _ ->
      {error, {bad_options, Errors}}
  end;
validate(Options, _AllPresent) ->
  {error, {bad_options, [{not_a_map, Options}]}}.

-spec check_bad_combination(speed_trap:options()) ->
                             ok |
                             {error,
                              {bad_options,
                               [{blocked, boolean()} | {enforce_rate_limit, boolean()}]}}.
check_bad_combination(Options) ->
  IsBlocked = is_blocked(Options),
  IsRateLimited = is_rate_limit_enforced(Options),
  case {IsBlocked, IsRateLimited} of
    {true, false} ->
      {error, {bad_options, [{blocked, IsBlocked}, {enforce_rate_limit, IsRateLimited}]}};
    _ ->
      ok
  end.

-spec is_blocked(speed_trap:options()) -> boolean().
is_blocked(Options) ->
  maps:get(blocked, Options, false).

-spec is_rate_limit_enforced(speed_trap:options()) -> boolean().
is_rate_limit_enforced(Options) ->
  maps:get(enforce_rate_limit, Options, true).

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
-spec validate_key(atom(), map(), fun((term()) -> ok | {error, tuple()}), boolean(), boolean()) ->
                    ok | {error, tuple()}.
validate_key(Key, Options, ValidationFn, Mandatory, Required) ->
  case maps:is_key(Key, Options) of
    false when Required andalso Mandatory ->
      {error, {key_missing, Key}};
    false when not Mandatory orelse not Required ->
      ok;
    true ->
      ValidationFn(maps:get(Key, Options))
  end.

-spec validate_bucket_size(term()) -> ok | {error, {bucket_size, term()}}.
validate_bucket_size(BucketSize) when is_integer(BucketSize), BucketSize >= 0 ->
  ok;
validate_bucket_size(BucketSize) ->
  {error, {bucket_size, BucketSize}}.

-spec validate_refill_interval(term()) -> ok | {error, {refill_interval, term()}}.
validate_refill_interval(RefillInterval) when is_integer(RefillInterval), RefillInterval > 0 ->
  ok;
validate_refill_interval(RefillInterval) ->
  {error, {refill_interval, RefillInterval}}.

-spec validate_refill_count(term()) -> ok | {error, {refill_count, term()}}.
validate_refill_count(RefillCount) when is_integer(RefillCount), RefillCount > 0 ->
  ok;
validate_refill_count(RefillCount) ->
  {error, {refill_count, RefillCount}}.

-spec validate_delete_when_full(term()) -> ok | {error, {delete_when_full, term()}}.
validate_delete_when_full(DeleteWhenFull) when is_boolean(DeleteWhenFull) ->
  ok;
validate_delete_when_full(DeleteWhenFull) ->
  {error, {delete_when_full, DeleteWhenFull}}.

-spec validate_enforce_rate_limit(term()) -> ok | {error, {enforce_rate_limit, term()}}.
validate_enforce_rate_limit(EnforceRateLimit) when is_boolean(EnforceRateLimit) ->
  ok;
validate_enforce_rate_limit(EnforceRateLimit) ->
  {error, {enforce_rate_limit, EnforceRateLimit}}.

-spec validate_blocked(term()) -> ok | {error, {blocked, term()}}.
validate_blocked(Blocked) when is_boolean(Blocked) ->
  ok;
validate_blocked(Blocked) ->
  {error, {blocked, Blocked}}.
