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
%%% @doc hcr_demo state server

-module(hcr_demo_state_server).
-include("hcr_demo.hrl").

-behaviour(gen_server).

%%%=============================================================================
%%% Exports
%%%=============================================================================

%% API
-export([start_link/0,
         trigger/0,
         get_call_counter/0,
         get_timed_counter/0]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         code_change/3,
         terminate/2]).

%%%=============================================================================
%%% Macros
%%%=============================================================================

-define(TIMEOUT, 5000). % 5 seconds

%%%=============================================================================
%%% Types
%%%=============================================================================

-type state() :: #{call_counter => integer(),
                   timed_counter => integer()}.

%%%=============================================================================
%%% API functions
%%%=============================================================================

%% @doc Start process.
-spec start_link() -> {ok, Pid} | ignore | {error, Reason} when
      Pid :: pid(),
      Reason :: term().
start_link() ->
    gen_server:start_link({local, ?MODULE},
                          ?MODULE,
                          #{},
                          [{timeout, ?TIMEOUT}]).

%% @doc Increment call counter.
-spec trigger() -> ok.
trigger() ->
    gen_server:cast(?MODULE, call_trigger).

%% @doc Return current call_counter value.
-spec get_call_counter() -> Value when
      Value :: integer().
get_call_counter() ->
    {ok, CallCounter} = gen_server:call(?MODULE, get_call_counter),
    CallCounter.

%% @doc Return current timed_counter value.
-spec get_timed_counter() -> Value when
      Value :: integer().
get_timed_counter() ->
    {ok, TimedCounter} = gen_server:call(?MODULE, get_timed_counter),
    TimedCounter.

%%%=============================================================================
%%% gen_server callbacks
%%%=============================================================================

%% @doc Initialize the server.
-spec init(Config :: #{}) ->
          {ok, state()} |
          {ok, state(), timeout()} |
          {stop, Reason :: term()} |
          ignore.
init(_Config = #{}) ->
    _TimerRef = erlang:send_after(1000, self(), timed_trigger),
    {ok, _State = #{call_counter => 0, timed_counter => 0}}.

%% @doc Handle synchronous requests.
-spec handle_call(Request :: term(),
                  From :: {pid(), Tag :: term()},
                  State :: state()) ->
          {reply, Reply :: term(), state()} |
          {reply, Reply :: term(), state(), timeout()} |
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), Reply :: term(), state()} |
          {stop, Reason :: term(), state()}.
handle_call(get_call_counter, _From, #{call_counter := CallCounter} = State) ->
    {reply, {ok, CallCounter}, State};
handle_call(get_timed_counter, _From, #{timed_counter := TimedCounter} = State) ->
    {reply, {ok, TimedCounter}, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%% @doc Handle asynchronous requests.
-spec handle_cast(Msg :: term(),
                  State :: state()) ->
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), state()}.
handle_cast(call_trigger, #{call_counter := CallCounter} = State) ->
    {noreply, State#{call_counter => CallCounter + 1}};
handle_cast(_Request, State) ->
    {noreply, State}.

%% @doc Handle any other message.
-spec handle_info(Info :: term(),
                  State :: state()) ->
          {noreply, state()} |
          {noreply, state(), timeout()} |
          {stop, Reason :: term(), state()}.
handle_info(timed_trigger, #{timed_counter := TimedCounter} = State) ->
    _TimerRef = erlang:send_after(1000, self(), timed_trigger),
    {noreply, State#{timed_counter => TimedCounter + 1}};
handle_info(_Info, State) ->
    {noreply, State}.

%% @doc Clean up the gen_server state.
-spec terminate(Reason :: term(),
                State :: state()) -> any().
terminate(_Reason, _State) ->
    ok.

%% @doc Convert process state when code is changed.
-spec code_change(OldVsn :: term() | {down, Vsn :: term()},
                  State :: state(),
                  Extra :: term()) ->
          {ok, NewState :: state()}.
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%=============================================================================
%%% Internal functions
%%%=============================================================================
