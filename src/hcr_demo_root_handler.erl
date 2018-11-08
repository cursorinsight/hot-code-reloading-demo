%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2018 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%%
%%% @doc hcr_demo root page handler.

-module(hcr_demo_root_handler).
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
                            <<"Hello world!\r\n">>,
                            Req0),
    {ok, Req1, State}.
