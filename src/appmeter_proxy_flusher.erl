%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

-module(appmeter_proxy_flusher).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
         start_flush_timer/1
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Proxy) ->
    gen_server:start_link(?MODULE, Proxy, []).

start_flush_timer(Pid) ->
    gen_server:cast(Pid, start_flush_timer).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {proxy,
                event_mgr,
                timer
               }).

init(Proxy) ->
    EventMgr = event_mgr(),
    {ok, #state{proxy=Proxy, event_mgr=EventMgr}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(start_flush_timer, State) ->
    State1 = manage_timer(State),
    {noreply, State1}.

handle_info(flush, State) ->
    State1 = flush(State),
    {noreply, State1}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

event_mgr() ->
    appmeter_event_mgr_sup:get_event_mgr().

manage_timer(State) ->
    case is_reference(State#state.timer) of
        true ->
            %% do nothing since the flush timer has already been started
            State;
        false ->
            Timer = erlang:send_after(flush_interval(), self(), flush),
            State#state{timer=Timer}
    end.

flush_interval() ->
    case application:get_env(appmeter, flush_interval) of
        {ok, V} ->
            V;
        _ ->
            100
    end.

flush(State) ->
    case appmeter_proxy:drain(State#state.proxy) of
        {ok, Data} ->
            try 
                appmeter_event_mgr:sync_notify(State#state.event_mgr, Data),
                State#state{timer=undefined}
            catch
                exit:noproc ->
                    %% TODO: retry?
                    EventMgr = event_mgr(),
                    State#state{timer=undefined, event_mgr=EventMgr}
            end;
        empty ->
            State#state{timer=undefined}
    end.
