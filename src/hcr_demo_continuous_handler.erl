%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2018 Cursor Insight
%%%
%%% All rights reserved.
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
