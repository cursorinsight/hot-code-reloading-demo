%%%-----------------------------------------------------------------------------
%%% Copyright (C) 2018 Cursor Insight
%%%
%%% All rights reserved.
%%%-----------------------------------------------------------------------------
%%%
%%% hcr_demo private header.

-ifndef(HCR_DEMO_PRIVATE_HRL).
-define(HCR_DEMO_PRIVATE_HRL, true).

-include("../include/hcr_demo.hrl").

-define(APPLICATION, hcr_demo).

-define(VERSION, (fun({_, _, V}) -> V;
                     (_) -> not_found
                  end(lists:keyfind(?APPLICATION, 1,
                                    application:loaded_applications())))).

-endif. % ifndef(HCR_DEMO_PRIVATE_HRL)
