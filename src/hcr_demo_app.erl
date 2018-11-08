%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2018 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%%
%%% @doc hcr_demo application module.

-module(hcr_demo_app).
-include("hcr_demo.hrl").

-behaviour(application).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% Application callbacks
-export([start/2, stop/1]).

%%%=============================================================================
%%% Application callbacks
%%%=============================================================================

%% @doc Start the application.
-spec start(StartType, StartArgs) -> {ok, Pid} |
                                     {ok, Pid, State} |
                                     {error, Reason} when
      StartType :: application:start_type(),
      StartArgs :: term(),
      Pid :: pid(),
      State :: term(),
      Reason :: term().
start(_StartType, _StartArgs) ->
    hcr_demo_sup:start_link().

%% @doc Stop the application.
-spec stop(State) -> ok when
      State :: term().
stop(_State) ->
    ok.
