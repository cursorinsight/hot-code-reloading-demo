%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2018 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%%
%%% @doc hcr_demo version page handler.

-module(hcr_demo_version_handler).
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
    ok = hcr_demo_state_server:trigger(),
    Req1 = cowboy_req:reply(200,
                            #{<<"content-type">> => <<"text/plain">>},
                            io_lib:format("VERSION: ~s~n", [?VERSION]),
                            Req0),
    {ok, Req1, State}.
