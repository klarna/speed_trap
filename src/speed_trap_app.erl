%%%=============================================================================
%%% @doc speed_trap_app
%%% @copyright 2023 Klarna Bank AB
%%% @end
%%%=============================================================================
-module(speed_trap_app).

-behaviour(application).

-export([start/2, stop/1]).

%%-----------------------------------------------------------------------------
%% application callbacks
%%-----------------------------------------------------------------------------

%% @doc Start the speed_trap application.
-spec start(application:start_type(), term()) -> {ok, pid()} | {error, term()}.
start(_StartType, _StartArgs) ->
  case speed_trap_template:init() of
    ok ->
      speed_trap_sup:start_link();
    {error, _} = Err ->
      Err
  end.

%% @doc Clean up after the speed_trap application has stopped.
-spec stop(term()) -> ok.
stop(_State) ->
  speed_trap_template:cleanup(),
  ok.

%%-----------------------------------------------------------------------------
%% Internal functions
%%-----------------------------------------------------------------------------
