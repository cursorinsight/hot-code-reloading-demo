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
%%% @doc hcr_demo continuous output handler

-module(hcr_demo_continuous_handler).
-include("hcr_demo.hrl").

-behaviour(cowboy_handler).

%%%=============================================================================
%%% Constants
%%%=============================================================================

-define(INTERVAL, 1000).

%%%=============================================================================
%%% Callback exports
%%%=============================================================================

-export([init/2,
         send_state/1]).

%%%=============================================================================
%%% Callback implementations
%%%=============================================================================

-spec init(Req, State) -> {ok, Req, State} when
      Req :: cowboy_req:req(),
      State :: term().
init(Req0, State) ->
    Req1 = cowboy_req:stream_reply(200,
                                   #{<<"content-type">> => <<"text/plain">>},
                                   Req0),
    _ = send_state(Req1),
    {ok, Req1, State}.

-spec send_state(Req) -> no_return when
      Req :: cowboy_req:req().
send_state(Req) ->
    cowboy_req:stream_body(
      [io_lib:format("VERSION: ~s, CALL COUNTER: ~3w, TIMED COUNTER: ~3w",
                     [?VERSION,
                      hcr_demo_state_server:get_call_counter(),
                      hcr_demo_state_server:get_timed_counter()]),
       <<"\r\n">>],
      nofin, Req),
    ok = timer:sleep(?INTERVAL),
    _ = hcr_demo_continuous_handler:send_state(Req).

%%%=============================================================================
%%% Ignoring dialyzer warnings
%%%=============================================================================

%% `send_state/1' is a right recursion that exits when its caller sends
%% `{'EXIT', ...}'.
-dialyzer({nowarn_function, init/2}).
