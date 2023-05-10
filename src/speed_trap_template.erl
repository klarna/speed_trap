-module(speed_trap_template).

-export([init/0, cleanup/0, options_from_id/1]).

-type id_pattern() :: {bucket_id_pattern(), id()}.
-type id() :: term().
-type bucket_id_pattern() :: term().

-define(PTERM_MATCHSPEC, {speed_trap, match_spec}).
-define(PTERM_TEMPLATES, {speed_trap, templates}).

%% ---------------------------------------------------------------------------
%% API
-spec init() -> ok | {error, term()}.
init() ->
  try
    Templates = application:get_env(speed_trap, templates, #{}),
    KeyPatterns = application:get_env(speed_trap, id_patterns, []),
    check_templates(Templates),
    check_id_patterns(KeyPatterns, Templates),
    MatchSpec = to_match_spec(KeyPatterns),
    persistent_term:put(?PTERM_MATCHSPEC, MatchSpec),
    persistent_term:put(?PTERM_TEMPLATES, Templates),
    ok
  catch
    throw:Reason:_Stack ->
      {error, Reason}
  end.

-spec cleanup() -> ok.
cleanup() ->
  persistent_term:erase(?PTERM_MATCHSPEC),
  persistent_term:erase(?PTERM_TEMPLATES),
  ok.

-spec options_from_id(speed_trap:id()) -> {ok, speed_trap:options()} | not_found.
options_from_id(Id) ->
  MatchSpec = persistent_term:get(?PTERM_MATCHSPEC),
  Templates = persistent_term:get(?PTERM_TEMPLATES),
  case template_from_id(Id, MatchSpec) of
    {ok, TemplateId} ->
      {ok, maps:get(TemplateId, Templates)};
    false ->
      not_found
  end.

%% ---------------------------------------------------------------------------
%% Internal

-spec check_id_patterns([id_pattern()], #{id() => speed_trap:options()}) -> ok.
check_id_patterns(IdPatterns, Templates) ->
  Patterns = lists:map(fun({Pattern, _}) -> Pattern end, IdPatterns),
  case Patterns -- lists:usort(Patterns) of
    [] ->
      ok;
    Dup ->
      erlang:throw({duplicate_id_patterns, Dup})
  end,
  lists:foreach(fun({_BucketPattern, TemplateId}) ->
                   case maps:is_key(TemplateId, Templates) of
                     true ->
                       ok;
                     false ->
                       erlang:throw({missing_template, TemplateId})
                   end
                end,
                IdPatterns).

check_templates(Templates) ->
  maps:foreach(fun(_Id, Options) -> ok = speed_trap_options:validate(Options, true) end, Templates).

-spec to_match_spec([id_pattern()]) -> ets:comp_match_spec() | undefined.
to_match_spec([]) ->
  undefined;
to_match_spec(KeyPatterns) ->
  Clauses =
    lists:map(fun({BucketIdPattern, TemplateId}) -> {BucketIdPattern, [], [TemplateId]} end,
              KeyPatterns),
  ets:match_spec_compile(Clauses).

-spec template_from_id(speed_trap:id(), ets:comp_match_spec() | undefined) -> false | {ok, id()}.
template_from_id(_Id, undefined) ->
  false;
template_from_id(Id, MS) ->
  case ets:match_spec_run([Id], MS) of
    [TemplateId] ->
      {ok, TemplateId};
    [] ->
      false
  end.
