-module(speed_trap_options).

-export([validate/2]).

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
                 {override, false, fun validate_override/1}]),
  case Errors of
    [] ->
      ok;
    _ ->
      {error, {bad_options, Errors}}
  end;
validate(Options, _AllPresent) ->
  {error, {bad_options, [{not_a_map, Options}]}}.

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

-spec validate_override(term()) -> ok | {error, {override, term()}}.
validate_override(none) ->
  ok;
validate_override(not_enforced) ->
  ok;
validate_override(blocked) ->
  ok;
validate_override(Override) ->
  {error, {override, Override}}.
