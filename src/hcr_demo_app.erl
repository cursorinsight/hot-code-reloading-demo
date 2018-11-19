%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2018 Cursor Insight Ltd.
%%%
%%% Licensed under the Apache License, Version 2.0 (the "License");
%%% you may not use this file except in compliance with the License.
%%% You may obtain a copy of the License at
%%%
%%% http://www.apache.org/licenses/LICENSE-2.0
%%%
%%% Unless required by applicable law or agreed to in writing, software
%%% distributed under the License is distributed on an "AS IS" BASIS,
%%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%%% See the License for the specific language governing permissions and
%%% limitations under the License.
%%%-----------------------------------------------------------------------------
%%%
%%% @doc hcr_demo application module.

-module(hcr_demo_app).
-include("hcr_demo.hrl").

-behaviour(application).

%%%=============================================================================
%%% Constants
%%%=============================================================================

-define(INFINITY, 24*60*60*1000). % == 1 day in milliseconds

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% Application callbacks
-export([start/2, stop/1]).

%% Cowboy route handlers
-export([get_routes/1,
         set_routes/1]).

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
    {ok, _} = start_http(),
    {ok, _} = hcr_demo_sup:start_link().

%% @doc Stop the application.
-spec stop(State) -> ok when
      State :: term().
stop(_State) ->
    ok.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================

%% @doc Set up and start `cowboy' routes and handlers.
-spec start_http() -> {ok, Pid} when
      Pid :: pid().
start_http() ->
    Dispatch = cowboy_router:compile([{'_', get_routes(?VERSION)}]),
    {ok, _} = cowboy:start_clear(http,
                                 [{port, 8080}],
                                 #{env => #{dispatch => Dispatch},
                                   idle_timeout => ?INFINITY,
                                   inactivity_timeout => ?INFINITY
                                  }).

-spec get_routes(Version) -> Routes when
      Version :: string(),
      Routes :: [{string(), module(), Options}],
      Options :: [term() | {atom(), term()}].
get_routes("0.2.0") ->
    get_routes("0.1.0") ++ [{"/v", hcr_demo_version_handler, []}];
get_routes("0.1.0") ->
    [{"/", hcr_demo_root_handler, []},
     {"/s", hcr_demo_state_handler, []},
     {"/f", hcr_demo_slow_file_handler, []},
     {"/c", hcr_demo_continuous_handler, []}].

-spec set_routes(Version) -> ok when
      Version :: string().
set_routes(Version) ->
    CompiledRoutes = cowboy_router:compile([{'_', get_routes(Version)}]),
    ok = cowboy:set_env(http, dispatch, CompiledRoutes).
