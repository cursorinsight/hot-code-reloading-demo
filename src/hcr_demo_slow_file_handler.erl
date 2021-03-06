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
%%% @doc hcr_demo slow file handler

-module(hcr_demo_slow_file_handler).
-include("hcr_demo.hrl").

-behaviour(cowboy_handler).

%%%=============================================================================
%%% Constants
%%%=============================================================================

-define(LIMIT_SPEED, 10000).
-define(TIME_RESOLUTION, 20).
-define(BIG_FILE, (filename:absname("big-file", code:lib_dir(hcr_demo, priv)))).

%%%=============================================================================
%%% Callback exports
%%%=============================================================================

-export([init/2,
         read_and_send/3]).

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

    {ok, IO} = file:open(?BIG_FILE, [read, binary]),
    ok = read_and_send(IO, trunc(?LIMIT_SPEED / ?TIME_RESOLUTION), Req1),
    {ok, Req1, State}.

-spec read_and_send(IO, Speed, Req) -> ok when
      IO :: file:io_device(),
      Speed :: non_neg_integer(),
      Req :: cowboy_req:req().
read_and_send(IO, Speed, Req) ->
    case file:read(IO, Speed) of
        {ok, Data} ->
            ok = cowboy_req:stream_body(Data, nofin, Req),
            ok = timer:sleep(trunc(1000 / ?TIME_RESOLUTION)),
            hcr_demo_slow_file_handler:read_and_send(IO, Speed, Req);
        eof ->
            cowboy_req:stream_body("", fin, Req),
            ok = file:close(IO)
    end.
