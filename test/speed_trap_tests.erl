-module(speed_trap_tests).

-include_lib("eunit/include/eunit.hrl").

-ifdef(TEST).

new_blocked_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id,
                   #{bucket_size => 10,
                     refill_interval => 1,
                     refill_count => 1,
                     delete_when_full => false,
                     override => blocked}),
  ?assertEqual({error, blocked}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

new_does_not_require_override_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Options =
    #{bucket_size => 10,
      refill_interval => 1,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new(Id, Options),
  ?assertEqual({ok, Options#{override => none}}, speed_trap:options(Id)),
  application:stop(speed_trap).

try_pass_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id,
                   #{bucket_size => 10,
                     refill_interval => 1,
                     refill_count => 1,
                     delete_when_full => false,
                     override => none}),
  ?assertEqual(expect_ok(10), [speed_trap:try_pass(Id) || _ <- lists:seq(1, 10)]),
  ?assertEqual({error, too_many_requests}, speed_trap:try_pass(Id)),
  timer:sleep(5),
  ?assertMatch({ok, _}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

try_pass_non_existing_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ?assertEqual({error, no_such_speed_trap}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

try_pass_all_test() ->
  application:ensure_all_started(speed_trap),
  Id1 = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id1,
                   #{bucket_size => 10,
                     refill_interval => 1,
                     refill_count => 10,
                     delete_when_full => false,
                     override => none}),
  Id2 = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id2,
                   #{bucket_size => 5,
                     refill_interval => 1,
                     refill_count => 1,
                     delete_when_full => false,
                     override => none}),
  Ids = [Id1, Id2],
  ?assertEqual(lists:duplicate(5, ok), [speed_trap:try_pass_all(Ids) || _ <- lists:seq(1, 5)]),
  ?assertEqual({error, Id2, too_many_requests}, speed_trap:try_pass_all(Ids)),
  ?assertEqual(expect_ok(5), [speed_trap:try_pass(Id1) || _ <- lists:seq(1, 5)]),
  ?assertEqual({error, too_many_requests}, speed_trap:try_pass(Id1)),
  application:stop(speed_trap).

try_pass_all_block_one_test() ->
  application:ensure_all_started(speed_trap),
  Id1 = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id1,
                   #{bucket_size => 10,
                     refill_interval => 1,
                     refill_count => 10,
                     delete_when_full => false,
                     override => none}),
  Id2 = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id2,
                   #{bucket_size => 10,
                     refill_interval => 1,
                     refill_count => 10,
                     delete_when_full => false,
                     override => none}),
  Ids = [Id1, Id2],
  ?assertEqual(lists:duplicate(5, ok), [speed_trap:try_pass_all(Ids) || _ <- lists:seq(1, 5)]),
  ok = speed_trap:block(Id2),
  ?assertEqual({error, Id2, blocked}, speed_trap:try_pass_all(Ids)),
  ?assertEqual({error, blocked}, speed_trap:try_pass(Id2)),
  ?assertEqual(expect_ok(5), [speed_trap:try_pass(Id1) || _ <- lists:seq(1, 5)]),
  ?assertEqual({error, too_many_requests}, speed_trap:try_pass(Id1)),
  application:stop(speed_trap).

already_existing_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Opts =
    #{bucket_size => 10,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ok = speed_trap:new(Id, Opts),
  ?assertEqual({error, already_exists}, speed_trap:new(Id, Opts)),
  application:stop(speed_trap).

delete_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Opts =
    #{bucket_size => 10,
      refill_interval => timer:seconds(1),
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ok = speed_trap:new(Id, Opts),
  ?assertEqual(ok, speed_trap:delete(Id)),
  application:stop(speed_trap).

delete_non_existing_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ?assertEqual({error, no_such_speed_trap}, speed_trap:delete(Id)),
  application:stop(speed_trap).

modify_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id,
                   #{bucket_size => 2,
                     refill_interval => 100,
                     refill_count => 1,
                     delete_when_full => false,
                     override => none}),
  ?assertEqual(expect_ok(2), [speed_trap:try_pass(Id) || _ <- lists:seq(1, 2)]),
  ?assertEqual({error, too_many_requests}, speed_trap:try_pass(Id)),
  ok = speed_trap:modify(Id, #{bucket_size => 100, refill_interval => 1}),
  ?assertEqual({ok,
                #{bucket_size => 100,
                  refill_interval => 1,
                  refill_count => 1,
                  delete_when_full => false,
                  override => none}},
               speed_trap:options(Id)),
  timer:sleep(10),
  ?assertMatch([{ok, _}, {ok, _}, {ok, _}, {ok, _}],
               [speed_trap:try_pass(Id) || _ <- lists:seq(1, 4)]),
  application:stop(speed_trap).

modify_bucket_no_change_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Opts =
    #{bucket_size => 2,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ok = speed_trap:new(Id, Opts),
  Timers = sys:get_state(speed_trap_token_bucket),
  ok = speed_trap:modify(Id, #{bucket_size => 2, refill_interval => 100}),
  ?assertEqual({ok, Opts}, speed_trap:options(Id)),
  ?assertEqual(Timers, sys:get_state(speed_trap_token_bucket)),
  application:stop(speed_trap).

modify_block_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id,
                   #{bucket_size => 10,
                     refill_interval => 100,
                     refill_count => 10,
                     delete_when_full => false,
                     override => none}),
  ?assertEqual(expect_ok(2, 10), [speed_trap:try_pass(Id) || _ <- lists:seq(1, 2)]),
  ok = speed_trap:block(Id),
  ?assertEqual({error, blocked}, speed_trap:try_pass(Id)),
  ?assertEqual({ok,
                #{bucket_size => 10,
                  refill_interval => 100,
                  refill_count => 10,
                  delete_when_full => false,
                  override => blocked}},
               speed_trap:options(Id)),
  speed_trap:modify(Id, #{override => none}),
  ?assertMatch({ok, _}, speed_trap:try_pass(Id)),
  ?assertEqual({ok,
                #{bucket_size => 10,
                  refill_interval => 100,
                  refill_count => 10,
                  delete_when_full => false,
                  override => none}},
               speed_trap:options(Id)),
  application:stop(speed_trap).

modify_reduce_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id,
                   #{bucket_size => 4,
                     refill_interval => 100,
                     refill_count => 1,
                     delete_when_full => false,
                     override => none}),
  ?assertEqual(expect_ok(4), [speed_trap:try_pass(Id) || _ <- lists:seq(1, 4)]),
  ?assertEqual({error, too_many_requests}, speed_trap:try_pass(Id)),
  ok = speed_trap:modify(Id, #{bucket_size => 2, refill_interval => 4}),
  ?assertEqual({ok,
                #{bucket_size => 2,
                  refill_interval => 4,
                  refill_count => 1,
                  delete_when_full => false,
                  override => none}},
               speed_trap:options(Id)),
  timer:sleep(10),
  ?assertEqual(expect_ok(2) ++ lists:duplicate(8, {error, too_many_requests}),
               [speed_trap:try_pass(Id) || _ <- lists:seq(1, 10)]),
  application:stop(speed_trap).

modify_bucket_size_change_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  %% Initiates with 100 tokens available
  ok =
    speed_trap:new(Id,
                   #{bucket_size => 100,
                     refill_interval => 1000,
                     refill_count => 1,
                     delete_when_full => false,
                     override => none}),
  %% Should reduce the count of tokens available in the bucket
  ok = speed_trap:modify(Id, #{bucket_size => 2}),
  ?assertEqual(expect_ok(2) ++ lists:duplicate(2, {error, too_many_requests}),
               [speed_trap:try_pass(Id) || _ <- lists:seq(1, 4)]),
  %% The bucket is empty but changing its size should refill it
  ok = speed_trap:modify(Id, #{bucket_size => 3}),
  ?assertEqual(expect_ok(3) ++ [{error, too_many_requests}],
               [speed_trap:try_pass(Id) || _ <- lists:seq(1, 4)]),
  application:stop(speed_trap).

use_template_test() ->
  Templates =
    #{t1 =>
        #{bucket_size => 5,
          refill_interval => 1000,
          refill_count => 1,
          delete_when_full => false,
          user_defined_setting => something,
          override => none}},
  KeyPatterns = [{{a, '_'}, t1}, {{'_', d}, t1}],
  application:set_env(speed_trap, templates, Templates),
  application:set_env(speed_trap, id_patterns, KeyPatterns),
  application:ensure_all_started(speed_trap),
  MatchingId1 = {a, b},
  MatchingId2 = {c, d},
  NotMatchingId = {e, f},
  ?assertEqual({ok, 4}, speed_trap:try_pass(MatchingId1)),
  ?assertMatch([{MatchingId1, #{template_id := t1, user_defined_setting := something}}],
               speed_trap:all(),
               "Template id stored in speed trap when started from template"),
  ?assertEqual({ok, 4}, speed_trap:try_pass(MatchingId2)),
  ?assertEqual({error, no_such_speed_trap}, speed_trap:try_pass(NotMatchingId)),
  application:stop(speed_trap),
  application:unset_env(speed_trap, id_patterns),
  application:unset_env(speed_trap, templates).

duplicate_id_patterns_test() ->
  Templates =
    #{t1 =>
        #{bucket_size => 5,
          refill_interval => 1000,
          refill_count => 1,
          delete_when_full => false,
          override => none}},
  KeyPatterns = [{{a, '_'}, t1}, {{a, '_'}, t1}],
  application:set_env(speed_trap, templates, Templates),
  application:set_env(speed_trap, id_patterns, KeyPatterns),
  ?assertMatch({error, {{duplicate_id_patterns, [{a, '_'}]}, _}}, application:start(speed_trap)),
  application:unset_env(speed_trap, id_patterns),
  application:unset_env(speed_trap, templates).

modify_non_existing_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Opts =
    #{bucket_size => 100,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, no_such_speed_trap}, speed_trap:modify(Id, Opts)),
  application:stop(speed_trap).

modify_error_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Opts =
    #{bucket_size => 100,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ok = speed_trap:new(Id, Opts),
  ?assertEqual({error, {bad_options, [{bucket_size, -1}]}},
               speed_trap:modify(Id, #{bucket_size => -1})),
  application:stop(speed_trap).

refill_count_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ok =
    speed_trap:new(Id,
                   #{bucket_size => 20,
                     refill_interval => 10,
                     refill_count => 10,
                     delete_when_full => false,
                     override => none}),
  ?assertEqual(expect_ok(20) ++ [{error, too_many_requests}],
               [speed_trap:try_pass(Id) || _ <- lists:seq(1, 21)]),
  timer:sleep(15),
  ?assertEqual(expect_ok(10) ++ [{error, too_many_requests}],
               [speed_trap:try_pass(Id) || _ <- lists:seq(1, 11)]),
  application:stop(speed_trap).

options_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Options =
    #{bucket_size => 10,
      refill_interval => 1,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ok = speed_trap:new(Id, Options),
  ?assertEqual({ok, Options}, speed_trap:options(Id)),
  application:stop(speed_trap).

all_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Options =
    #{bucket_size => 10,
      refill_interval => 100000,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ExpectedValues = Options#{tokens => 10},
  ok = speed_trap:new(Id, Options),
  ?assertMatch([{Id, ExpectedValues}], speed_trap:all()),
  application:stop(speed_trap).

bad_options_test() ->
  application:ensure_all_started(speed_trap),
  BadOpts1 =
    #{bucket_size => 2,
      refill_interval => 0,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{refill_interval, 0}]}}, speed_trap:new(<<"id">>, BadOpts1)),
  BadOpts2 =
    #{bucket_size => <<"two">>,
      refill_interval => 0,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{refill_interval, 0}, {bucket_size, <<"two">>}]}},
               speed_trap:new(<<"id">>, BadOpts2)),
  BadOpts3 =
    #{bucket_size => 2,
      refill_interval => -1,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{refill_interval, -1}]}}, speed_trap:new(<<"id">>, BadOpts3)),
  BadOpts4 =
    #{bucket_size => -1,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{bucket_size, -1}]}}, speed_trap:new(<<"id">>, BadOpts4)),
  BadOpts5 =
    #{bucket_size => 1,
      refill_interval => 100,
      refill_count => 0,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{refill_count, 0}]}}, speed_trap:new(<<"id">>, BadOpts5)),
  BadOpts6 =
    #{bucket_size => 1,
      refill_interval => 100,
      refill_count => none,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{refill_count, none}]}}, speed_trap:new(<<"id">>, BadOpts6)),
  BadOpts7 =
    #{refill_interval => 100,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{key_missing, bucket_size}]}},
               speed_trap:new(<<"id">>, BadOpts7)),
  BadOpts8 =
    #{bucket_size => 100,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{key_missing, refill_interval}]}},
               speed_trap:new(<<"id">>, BadOpts8)),
  BadOpts9 =
    #{bucket_size => 100,
      refill_interval => 1,
      delete_when_full => false,
      override => none},
  ?assertEqual({error, {bad_options, [{key_missing, refill_count}]}},
               speed_trap:new(<<"id">>, BadOpts9)),
  BadOpts10 = {not_a_map, at_all},
  ?assertEqual({error, {bad_options, [{not_a_map, BadOpts10}]}},
               speed_trap:new(<<"id">>, BadOpts10)),
  BadOpts11 = #{},
  ?assertEqual({error,
                {bad_options,
                 [{key_missing, delete_when_full},
                  {key_missing, refill_count},
                  {key_missing, refill_interval},
                  {key_missing, bucket_size}]}},
               speed_trap:new(<<"id">>, BadOpts11)),
  BadOpts12 =
    #{bucket_size => 1,
      refill_interval => 100,
      refill_count => 1,
      override => none},
  ?assertEqual({error, {bad_options, [{key_missing, delete_when_full}]}},
               speed_trap:new(<<"id">>, BadOpts12)),
  BadOpts13 =
    #{bucket_size => 1,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => foo},
  ?assertEqual({error, {bad_options, [{delete_when_full, foo}]}},
               speed_trap:new(<<"id">>, BadOpts13)),
  BadOpts14 =
    #{bucket_size => 1,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => false,
      override => foo},
  ?assertEqual({error, {bad_options, [{override, foo}]}}, speed_trap:new(<<"id">>, BadOpts14)),
  application:stop(speed_trap).

delete_when_full_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Options =
    #{bucket_size => 10,
      refill_interval => 10,
      refill_count => 1,
      delete_when_full => true,
      override => none},
  ok = speed_trap:new(Id, Options),
  ?assertEqual({ok, 9}, speed_trap:try_pass(Id)),
  timer:sleep(50),
  ?assertEqual({error, no_such_speed_trap}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

enforce_rate_limit_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Options =
    #{bucket_size => 1,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false,
      override => none},
  ok = speed_trap:new(Id, Options),
  ?assertEqual({ok, 0}, speed_trap:try_pass(Id)),
  ?assertEqual({error, too_many_requests}, speed_trap:try_pass(Id)),
  ok = speed_trap:modify(Id, #{override => not_enforced}),
  ?assertEqual({ok, 0}, speed_trap:try_pass(Id)),
  ?assertEqual({ok, rate_limit_not_enforced}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

block_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  Options =
    #{bucket_size => 100,
      refill_interval => 1000,
      refill_count => 100,
      delete_when_full => false,
      override => none},
  ok = speed_trap:new(Id, Options),
  ?assertEqual({ok, 99}, speed_trap:try_pass(Id)),
  ?assertEqual(ok, speed_trap:block(Id)),
  ?assertEqual({error, blocked}, speed_trap:try_pass(Id)),
  ?assertEqual(ok, speed_trap:unblock(Id)),
  ?assertEqual({ok, 99}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

block_non_existing_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ?assertEqual({error, no_such_speed_trap}, speed_trap:block(Id)),
  application:stop(speed_trap).

return_token_adds_an_additional_token_to_the_bucket_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  BucketSize = 100,
  RefillCount = 10,
  DeleteWhenFull = false,
  Options =
    #{bucket_size => BucketSize,
      refill_interval => 25_000,
      refill_count => RefillCount,
      delete_when_full => DeleteWhenFull,
      override => none},
  ok = speed_trap:new(Id, Options),
  ?assertEqual(expect_ok(BucketSize), [speed_trap:try_pass(Id) || _ <- lists:seq(1, BucketSize)]),
  {ok, {_Options, Ctr}} = speed_trap_token_bucket:bucket(Id),
  %% We don't actually need to wait for the token to be refilled but
  %% can just pretend the timer fired this is good enough to ensure a
  %% semi-concurrent call of return_token/1 does not set the bucket to 1
  speed_trap_token_bucket:add_token(Ctr, BucketSize, RefillCount, DeleteWhenFull),
  [{Id, OptsWithToken}] = speed_trap:all(),
  ?assertEqual(RefillCount, maps:get(tokens, OptsWithToken)),
  ok = speed_trap_token_bucket:return_token(Id),
  [{Id, NewOptsWithToken}] = speed_trap:all(),
  ?assertEqual(RefillCount + 1, maps:get(tokens, NewOptsWithToken)),
  application:stop(speed_trap).

return_token_adds_an_additional_token_to_the_bucket_and_can_still_be_deleted_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  BucketSize = 100,
  RefillCount = 100,
  DeleteWhenFull = false,
  Options =
    #{bucket_size => BucketSize,
      refill_interval => 25_000,
      refill_count => RefillCount,
      delete_when_full => DeleteWhenFull,
      override => none},
  ok = speed_trap:new(Id, Options),
  ?assertEqual(expect_ok(BucketSize), [speed_trap:try_pass(Id) || _ <- lists:seq(1, BucketSize)]),
  {ok, {_Options, Ctr}} = speed_trap_token_bucket:bucket(Id),
  %% We don't actually need to wait for the token to be refilled but
  %% can just pretend the timer fired this is good enough to ensure a
  %% semi-concurrent call of return_token/1 does not set the bucket to 1
  speed_trap_token_bucket:add_token(Ctr, BucketSize, RefillCount, DeleteWhenFull),
  [{Id, OptsWithToken}] = speed_trap:all(),
  ?assertEqual(RefillCount, maps:get(tokens, OptsWithToken)),
  ok = speed_trap_token_bucket:return_token(Id),
  [{Id, NewOptsWithToken}] = speed_trap:all(),
  ?assertEqual(RefillCount + 1, maps:get(tokens, NewOptsWithToken)),
  NewDeleteWhenFull = true,
  speed_trap:modify(Id, #{delete_when_full => NewDeleteWhenFull}),
  speed_trap_token_bucket:add_token(Ctr, BucketSize, RefillCount, NewDeleteWhenFull),
  ?assertEqual([], speed_trap:all()),
  application:stop(speed_trap).

template_based_overrides_test() ->
  Templates =
    #{t1 =>
        #{bucket_size => 5,
          refill_interval => 1000,
          refill_count => 1,
          delete_when_full => true,
          override => none}},
  KeyPatterns = [{{a, '_'}, t1}, {{'_', d}, t1}],
  application:set_env(speed_trap, templates, Templates),
  application:set_env(speed_trap, id_patterns, KeyPatterns),
  application:ensure_all_started(speed_trap),
  Id = {a, b},
  NewBucketSize = 3,
  %% Modify a non-existent bucket, which can be created from the template
  ?assertEqual(ok, speed_trap:modify(Id, #{bucket_size => NewBucketSize})),
  %% Make sure it is not active
  ?assertEqual([], speed_trap:all()),
  %% Check that the override is stored
  ?assertMatch({ok, #{bucket_size := NewBucketSize}}, speed_trap_token_bucket:get_override(Id)),
  ?assertEqual({ok, NewBucketSize - 1}, speed_trap:try_pass(Id)),
  %% Delete the override only
  ?assertEqual(ok, speed_trap:delete_override(Id)),
  %% The token bucket must be present still
  ?assertMatch([{Id, _}], speed_trap:all()),
  NewRefillInt = 10,
  ?assertEqual(ok, speed_trap:modify(Id, #{refill_interval => NewRefillInt})),
  %% Let speed_trap perform delete_when_full
  timer:sleep(50),
  ?assertEqual([], speed_trap:all()),
  %% The overrides should be stored still
  ?assertMatch({ok, #{refill_interval := NewRefillInt}}, speed_trap_token_bucket:get_override(Id)),
  ?assertEqual(ok, speed_trap_token_bucket:delete_overrides()),
  ?assertEqual({error, not_found}, speed_trap_token_bucket:get_override(Id)),
  %% Test that manual deletion deletes an override
  ?assertEqual(ok,
               speed_trap:modify(Id, #{bucket_size => NewBucketSize, delete_when_full => false})),
  ?assertEqual([], speed_trap:all()),
  ?assertMatch({ok, #{bucket_size := NewBucketSize}}, speed_trap_token_bucket:get_override(Id)),
  %% Still returns error that there is no such speed_trap
  ?assertEqual({error, no_such_speed_trap}, speed_trap:delete(Id)),
  %% But deletes modification
  ?assertEqual({error, not_found}, speed_trap_token_bucket:get_override(Id)),
  application:stop(speed_trap),
  application:unset_env(speed_trap, id_patterns),
  application:unset_env(speed_trap, templates).

dynamic_rate_limiter_bad_options_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  %% Invalid: negative threshold
  BadOpts1 =
    #{min_bucket_size => 20,
      max_bucket_size => 50,
      scaling_time_interval => timer:seconds(1),
      rejection_rate_threshold => -10,
      scaling_bucket_size_adjust_count => 5,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ?assertMatch({error, {bad_options, _}}, speed_trap:new_dynamic(Id, BadOpts1)),
  %% Invalid: threshold > 100
  BadOpts2 =
    #{min_bucket_size => 20,
      max_bucket_size => 50,
      scaling_time_interval => timer:seconds(1),
      rejection_rate_threshold => 150,
      scaling_bucket_size_adjust_count => 5,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ?assertMatch({error, {bad_options, _}}, speed_trap:new_dynamic(Id, BadOpts2)),
  application:stop(speed_trap).

dynamic_rate_limiter_delete_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  DynamicOpts =
    #{min_bucket_size => 20,
      max_bucket_size => 50,
      scaling_time_interval => timer:seconds(1),
      rejection_rate_threshold => 30,
      scaling_bucket_size_adjust_count => 5,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  ok = speed_trap:delete_dynamic(Id),
  ?assertEqual({error, no_such_speed_trap}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

dynamic_rate_limiter_basic_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  DynamicOpts =
    #{min_bucket_size => 20,
      max_bucket_size => 50,
      scaling_time_interval => timer:seconds(10),
      rejection_rate_threshold => 30,
      scaling_bucket_size_adjust_count => 5,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Initially starts at min_bucket_size (20)
  ?assertMatch({ok, _}, speed_trap:try_pass(Id)),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test dynamic rate limiter upscaling when rejection rate exceeds threshold
dynamic_rate_limiter_upscaling_detailed_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  InitialBucketSize = 10,
  ScalingAdjustCount = 5,
  DynamicOpts =
    #{min_bucket_size => InitialBucketSize,
      max_bucket_size => 30,
      scaling_time_interval => 200,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => ScalingAdjustCount,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Get initial bucket size
  {ok, InitialOpts} = speed_trap:options(Id),
  ?assertEqual(InitialBucketSize, maps:get(bucket_size, InitialOpts)),
  %% Generate high rejection rate by exhausting bucket multiple times
  %% This should trigger upscaling
  [speed_trap:try_pass(Id) || _ <- lists:seq(1, 50)],
  %% Wait for scaling interval plus margin
  timer:sleep(300),
  %% Check that bucket size has increased
  {ok, FinalOpts} = speed_trap:options(Id),
  FinalBucketSize = maps:get(bucket_size, FinalOpts),
  ?assert(FinalBucketSize > InitialBucketSize),
  ?assertEqual(InitialBucketSize + ScalingAdjustCount, FinalBucketSize),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test dynamic rate limiter downscaling when rejection rate is below threshold
dynamic_rate_limiter_downscaling_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ScalingAdjustCount = 3,
  DynamicOpts =
    #{min_bucket_size => 5,
      max_bucket_size => 20,
      scaling_time_interval => 200,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => ScalingAdjustCount,
      refill_interval => 100,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Manually increase bucket size to simulate previous upscaling
  UpScaledBucketSize = 15,
  ok = speed_trap:modify(Id, #{bucket_size => UpScaledBucketSize}),
  {ok, InitialOpts} = speed_trap:options(Id),
  ?assertEqual(UpScaledBucketSize, maps:get(bucket_size, InitialOpts)),
  %% Generate low rejection rate by making successful requests
  %% Make requests that are well within the bucket capacity
  [speed_trap:try_pass(Id) || _ <- lists:seq(1, 3)],
  %% Wait for scaling interval plus margin
  timer:sleep(300),
  %% Check that bucket size has decreased
  {ok, FinalOpts} = speed_trap:options(Id),
  FinalBucketSize = maps:get(bucket_size, FinalOpts),
  ?assert(FinalBucketSize < UpScaledBucketSize),
  ?assertEqual(UpScaledBucketSize - ScalingAdjustCount, FinalBucketSize),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test that bucket size never goes below minimum
dynamic_rate_limiter_minimum_bounds_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  MinBucketSize = 10,
  DynamicOpts =
    #{min_bucket_size => MinBucketSize,
      max_bucket_size => 20,
      scaling_time_interval => 200,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => 10, %% Large adjustment to test bounds
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Generate very low rejection rate to trigger downscaling
  lists:foreach(fun(_) ->
                   %% Make only successful requests
                   [speed_trap:try_pass(Id) || _ <- lists:seq(1, 2)],
                   timer:sleep(50)
                end,
                lists:seq(1, 10)),
  %% Wait for multiple scaling intervals
  timer:sleep(500),
  %% Check that bucket size never went below minimum
  {ok, FinalOpts} = speed_trap:options(Id),
  FinalBucketSize = maps:get(bucket_size, FinalOpts),
  ?assertEqual(FinalBucketSize, MinBucketSize),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test that bucket size never exceeds maximum
dynamic_rate_limiter_maximum_bounds_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  MaxBucketSize = 15,
  DynamicOpts =
    #{min_bucket_size => 5,
      max_bucket_size => MaxBucketSize,
      scaling_time_interval => 200,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => 10, %% Large adjustment to test bounds
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Generate very high rejection rate to trigger upscaling
  lists:foreach(fun(_) ->
                   %% Exhaust bucket multiple times to generate high rejection rate
                   [speed_trap:try_pass(Id) || _ <- lists:seq(1, 20)]
                end,
                lists:seq(1, 10)),
  %% Wait for scaling interval
  timer:sleep(300),
  %% Check that bucket size never exceeded maximum
  {ok, FinalOpts} = speed_trap:options(Id),
  FinalBucketSize = maps:get(bucket_size, FinalOpts),
  ?assertEqual(FinalBucketSize, MaxBucketSize),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test dynamic rate limiter with no requests (should not change bucket size)
dynamic_rate_limiter_no_requests_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  InitialBucketSize = 10,
  DynamicOpts =
    #{min_bucket_size => InitialBucketSize,
      max_bucket_size => 20,
      scaling_time_interval => 200,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => 5,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Don't make any requests, just wait
  timer:sleep(300),
  %% Check that bucket size hasn't changed
  {ok, FinalOpts} = speed_trap:options(Id),
  FinalBucketSize = maps:get(bucket_size, FinalOpts),
  ?assertEqual(InitialBucketSize, FinalBucketSize),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test dynamic rate limiter with all successes (should downscale)
dynamic_rate_limiter_all_successes_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ScalingCount = 3,
  DynamicOpts =
    #{min_bucket_size => 5,
      max_bucket_size => 15,
      scaling_time_interval => 200,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => ScalingCount,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Manually increase bucket size first
  ok = speed_trap:modify(Id, #{bucket_size => 12}),
  {ok, InitialOpts} = speed_trap:options(Id),
  InitialBucketSize = maps:get(bucket_size, InitialOpts),
  ?assertEqual(12, InitialBucketSize),
  %% Generate 0% rejection rate by making only successful requests
  lists:foreach(fun(_) ->
                   %% Make only a few requests that will all succeed
                   speed_trap:try_pass(Id),
                   timer:sleep(50)
                end,
                lists:seq(1, 8)),
  %% Wait for scaling interval
  timer:sleep(300),
  %% Check that bucket size has decreased
  {ok, FinalOpts} = speed_trap:options(Id),
  FinalBucketSize = maps:get(bucket_size, FinalOpts),
  %% After (8 * 50 + 300)/200 = at least 3 downscaling intervals have happened => 12 - (3 * 3) = 3
  %% However, we never drop below 5.
  ?assertEqual(FinalBucketSize, 5),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test dynamic rate limiter adjustment timing
dynamic_rate_limiter_timing_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ScalingInterval = 300,
  DynamicOpts =
    #{min_bucket_size => 5,
      max_bucket_size => 15,
      scaling_time_interval => ScalingInterval,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => 3,
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  StartTime = erlang:monotonic_time(millisecond),
  %% Generate high rejection rate
  [speed_trap:try_pass(Id) || _ <- lists:seq(1, 20)],
  %% Check that no adjustment happened before the scaling interval
  timer:sleep(ScalingInterval - 100),
  {ok, MidOpts} = speed_trap:options(Id),
  MidBucketSize = maps:get(bucket_size, MidOpts),
  ?assertEqual(5, MidBucketSize),
  %% Wait for the full scaling interval
  timer:sleep(200),
  %% Check that adjustment happened after the scaling interval
  {ok, FinalOpts} = speed_trap:options(Id),
  FinalBucketSize = maps:get(bucket_size, FinalOpts),
  ?assert(FinalBucketSize > 5),
  EndTime = erlang:monotonic_time(millisecond),
  ElapsedTime = EndTime - StartTime,
  ?assert(ElapsedTime >= ScalingInterval),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

%% Test dynamic rate limiter with gradual scaling
dynamic_rate_limiter_gradual_scaling_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  DynamicOpts =
    #{min_bucket_size => 5,
      max_bucket_size => 20,
      scaling_time_interval => 200,
      rejection_rate_threshold => 50,
      scaling_bucket_size_adjust_count => 2, %% Small adjustments
      refill_interval => 1000,
      refill_count => 1,
      delete_when_full => false},
  ok = speed_trap:new_dynamic(Id, DynamicOpts),
  %% Generate moderate rejection rate over multiple intervals
  lists:foreach(fun(Round) ->
                   %% Generate some rejections
                   [speed_trap:try_pass(Id) || _ <- lists:seq(1, 30)],
                   %% Wait for scaling interval
                   timer:sleep(210),
                   %% Check bucket size after each round
                   {ok, Opts} = speed_trap:options(Id),
                   BucketSize = maps:get(bucket_size, Opts),
                   ExpectedMinSize = 5 + Round * 2,
                   ?assert(BucketSize >= ExpectedMinSize)
                end,
                lists:seq(1, 3)),
  ok = speed_trap:delete_dynamic(Id),
  application:stop(speed_trap).

unique_id(Name) ->
  {Name, unique_resource()}.

expect_ok(X) ->
  lists:map(fun(N) -> {ok, N - 1} end, lists:reverse(lists:seq(1, X))).

expect_ok(X, Max) ->
  lists:map(fun(N) -> {ok, N - 1} end, lists:reverse(lists:seq(Max - X + 1, Max))).

unique_resource() ->
  Int = erlang:unique_integer([positive]),
  <<"resource", (integer_to_binary(Int))/binary>>.

%%
-endif.
