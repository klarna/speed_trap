-module(speed_trap_options).

-export([validate/2]).

-type bad_options() :: {bad_options, [tuple(), ...]}.

-export_type([bad_options/0]).

-spec validate(term(), boolean()) -> ok | {error, bad_options()}.
validate(Options, AllRequired) when is_map(Options) ->
  DynamicOptions =
    case maps:get(dynamic_rate_limiter, Options, false) of
      false ->
        [];
      true ->
        [{max_bucket_size, true, fun validate_limit_bucket_size/2},
         {scaling_time_interval, true, fun validate_scaling_time_interval/1},
         {rejection_rate_threshold, true, fun validate_rejection_rate_threshold/1},
         {scaling_bucket_size_adjust_count, true, fun validate_scaling_bucket_size_adjust_count/1}]
    end,
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
                 {override, false, fun validate_override/1}]
                ++ DynamicOptions),
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
      case erlang:fun_info(ValidationFn, arity) of
        {arity, 1} ->
          ValidationFn(maps:get(Key, Options));
        {arity, 2} ->
          ValidationFn(maps:get(Key, Options), Options)
      end
  end.

-spec validate_bucket_size(term()) -> ok | {error, {bucket_size, term()}}.
validate_bucket_size(BucketSize) when is_integer(BucketSize), BucketSize >= 0 ->
  ok;
validate_bucket_size(BucketSize) ->
  {error, {bucket_size, BucketSize}}.

validate_limit_bucket_size(MaxBucketSize, Options) ->
  MinBucketSize = maps:get(min_bucket_size, Options),
  AreNumbers = is_integer(MinBucketSize) andalso is_integer(MaxBucketSize),
  WithinBoundaries =
    MinBucketSize < MaxBucketSize andalso MinBucketSize >= 0 andalso MaxBucketSize >= 0,
  case AreNumbers andalso WithinBoundaries of
    true ->
      ok;
    false ->
      {error, {limit_bucket_size, {MinBucketSize, MaxBucketSize}}}
  end.

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

validate_scaling_time_interval(TimeInterval) when is_integer(TimeInterval), TimeInterval > 0 ->
  ok;
validate_scaling_time_interval(TimeInterval) ->
  {error, {invalid_time_interval, TimeInterval}}.

validate_rejection_rate_threshold(Threshold)
  when is_number(Threshold), Threshold > 0, Threshold =< 100 ->
  ok;
validate_rejection_rate_threshold(Threshold) ->
  {error, {invalid_threshold, Threshold}}.

validate_scaling_bucket_size_adjust_count(AdjustCount)
  when is_integer(AdjustCount), AdjustCount > 0 ->
  ok;
validate_scaling_bucket_size_adjust_count(AdjustCount) ->
  {error, {invalid_scaling_bucket_size_adjust_count, AdjustCount}}.
