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

-module(hcr_demo_SUITE).
-include("hcr_demo.hrl").

-include_lib("eunit/include/eunit.hrl").

%%%=============================================================================
%%% Exports
%%%=============================================================================

-compile(export_all).
-compile(nowarn_export_all).

%%%=============================================================================
%%% Types
%%%=============================================================================

-type config() :: [tuple()].
-type test_case() :: atom().

%%%=============================================================================
%%% CT callback
%%%=============================================================================

-spec all() -> [test_case()].
all() ->
    [basic_test].

%%%-----------------------------------------------------------------------------
%%% Test suite init/end
%%%-----------------------------------------------------------------------------

%% @doc Initialize before the test suite.
-spec init_per_suite(config()) -> config().
init_per_suite(Config) ->
    Config.

%% @doc Clean up after the test suite.
-spec end_per_suite(config()) -> ok.
end_per_suite(_Config) ->
    ok.

%%%-----------------------------------------------------------------------------
%%% Test case init/end
%%%-----------------------------------------------------------------------------

%% @doc Initialize before a test case.
init_per_testcase(_Testcase, Config) ->
    Config.

%% @doc Clean up after a test case.
end_per_testcase(_Testcase, _Config)->
    ok.

%%%=============================================================================
%%% Test cases
%%%=============================================================================

%% @doc Empty test case.
-spec basic_test(config()) -> ok.
basic_test(_Config) ->
    ?assertEqual(ok, ok),
    ok.

%%%=============================================================================
%%% Helper functions
%%%=============================================================================
