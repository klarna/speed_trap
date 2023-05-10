%%%=============================================================================
%%% @doc speed_trap top level supervisor.
%%% @copyright 2023 Klarna Bank AB
%%% @end
%%%=============================================================================
-module(speed_trap_sup).

%% API
-export([start_link/0]).

-behaviour(supervisor).

-export([init/1]).

%% The registered name of the supervisor process.
-define(SERVER, ?MODULE).

%%-----------------------------------------------------------------------------
%% API
%%-----------------------------------------------------------------------------

%% @doc Start the top level supervisor.
-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
  supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%-----------------------------------------------------------------------------
%% supervisor callbacks
%%-----------------------------------------------------------------------------

-spec init(_Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}}.
init([]) ->
  SupFlags =
    #{strategy => one_for_all,
      intensity => 0,
      period => 1},
  ChildSpecs = [child(speed_trap_token_bucket)],
  {ok, {SupFlags, ChildSpecs}}.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------

child(Mod) ->
  #{id => Mod, start => {Mod, start_link, []}}.
