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
%%% @doc hcr_demo top level supervisor.

-module(hcr_demo_sup).
-include("hcr_demo.hrl").

-behaviour(supervisor).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% API
-export([start_link/0]).

%% Supervisor callback
-export([init/1]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(TIMEOUT, 5000). % 5 seconds

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% @doc Start the supervisor.
-spec start_link() -> {ok, Pid} | ignore | {error, Reason} when
      Pid :: pid(),
      Reason :: {already_started, pid()} | {shutdown, term()} | term().
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, _Config = #{}).

%%%=============================================================================
%%% Supervisor callbacks
%%%=============================================================================

%% @doc Return the supervisor's initial configuration.
-spec init(Config) -> {ok, {SupFlags, ChildSpecs}} | ignore when
      Config :: #{},
      SupFlags :: supervisor:sup_flags(),
      ChildSpecs :: [supervisor:child_spec()].
init(_Config = #{}) ->
    SupFlags = #{strategy => one_for_one,
                 intensity => 3,
                 period => 1},
    ChildSpecs = [#{id => hcr_demo_state_server,
                    start => {hcr_demo_state_server, start_link, []},
                    restart => permanent,
                    shutdown => 1000,
                    type => supervisor}],
    {ok, {SupFlags, ChildSpecs}}.
