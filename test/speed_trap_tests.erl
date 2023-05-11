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
  %% The bucket is empty currently and increasing its size should not change that
  ok = speed_trap:modify(Id, #{bucket_size => 100}),
  ?assertEqual({error, too_many_requests}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

use_template_test() ->
  Templates =
    #{t1 =>
        #{bucket_size => 5,
          refill_interval => 1000,
          refill_count => 1,
          delete_when_full => false,
          override => none}},
  KeyPatterns = [{{a, '_'}, t1}, {{'_', d}, t1}],
  application:set_env(speed_trap, templates, Templates),
  application:set_env(speed_trap, id_patterns, KeyPatterns),
  application:ensure_all_started(speed_trap),
  MatchingId1 = {a, b},
  MatchingId2 = {c, d},
  NotMatchingId = {e, f},
  ?assertEqual({ok, 4}, speed_trap:try_pass(MatchingId1)),
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
  ?assertEqual({ok, 98}, speed_trap:try_pass(Id)),
  application:stop(speed_trap).

block_non_existing_test() ->
  application:ensure_all_started(speed_trap),
  Id = unique_id(?FUNCTION_NAME),
  ?assertEqual({error, no_such_speed_trap}, speed_trap:block(Id)),
  application:stop(speed_trap).

is_blocked_test() ->
  ?assertEqual(true, speed_trap_options:is_blocked(#{override => blocked})),
  ?assertEqual(false, speed_trap_options:is_blocked(#{override => not_enforced})),
  ?assertEqual(false, speed_trap_options:is_blocked(#{override => none})).

is_rate_limit_enforced_test() ->
  ?assertEqual(false, speed_trap_options:is_rate_limit_enforced(#{override => not_enforced})),
  ?assertEqual(true, speed_trap_options:is_rate_limit_enforced(#{override => blocked})),
  ?assertEqual(true, speed_trap_options:is_rate_limit_enforced(#{override => none})).

unique_id(Name) ->
  {Name, unique_resource()}.

expect_ok(X) ->
  lists:map(fun(N) -> {ok, N - 1} end, lists:reverse(lists:seq(1, X))).

expect_ok(X, Max) ->
  lists:map(fun(N) -> {ok, N - 1} end, lists:reverse(lists:seq(Max - X + 1, Max))).

unique_resource() ->
  Int = erlang:unique_integer([positive]),
  <<"resource", (integer_to_binary(Int))/binary>>.

-endif.
