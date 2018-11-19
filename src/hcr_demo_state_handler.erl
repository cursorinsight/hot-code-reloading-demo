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
%%% @doc hcr_demo state handler, i.e. return information regarding The State.

-module(hcr_demo_state_handler).
-include("hcr_demo.hrl").

-behaviour(cowboy_handler).

%%%=============================================================================
%%% Callback exports
%%%=============================================================================

-export([init/2]).

%%%=============================================================================
%%% Callback implementations
%%%=============================================================================

-spec init(Req, State) -> {ok, Req, State} when
      Req :: cowboy_req:req(),
      State :: term().
init(Req0, State) ->
    Req1 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>},
                            [io_lib:format("VERSION: ~s", [?VERSION]),
                             <<"\r\n">>,
                             io_lib:format(
                               "CALL COUNTER: ~w",
                               [hcr_demo_state_server:get_call_counter()]),
                             <<"\r\n">>,
                             io_lib:format(
                               "TIMED COUNTER: ~w",
                               [hcr_demo_state_server:get_timed_counter()]),
                             <<"\r\n">>],
                            Req0),
    {ok, Req1, State}.
