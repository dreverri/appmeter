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

-module(appmeter_proxy).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         notify/2,
         reload/1,
         drain/1
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

start_link() ->
    gen_server:start_link(?MODULE, [], []).

notify(Pid, Event) ->
    gen_server:cast(Pid, {notify, Event}).

reload(Pid) ->
    gen_server:cast(Pid, reload).

drain(Pid) ->
    gen_server:call(Pid, drain, infinity).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

-record(state, {flusher,
                max_sample_size,
                sample_size=0,
                population_size=0,
                acc=[]
               }).

init(_) ->
    case appmeter_proxy_flusher:start_link(self()) of
        {ok, Flusher} ->
            State = load_env(#state{flusher=Flusher}),
            {ok, State};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_call(drain, _From, State) ->
    case State#state.acc of
        [] ->
            {reply, empty, State};
        Acc ->
            SampleRate = State#state.sample_size / State#state.population_size,
            State1 = State#state{acc=[], sample_size=0, population_size=0},
            {reply, {ok, {lists:append(Acc), SampleRate}}, State1}
    end.

handle_cast(reload, State) ->
    State1 = load_env(State),
    {noreply, State1};

handle_cast({notify, Event}, State) ->
    PopulationSize = State#state.population_size + 1,
    case PopulationSize of
        1 ->
            appmeter_proxy_flusher:start_flush_timer(State#state.flusher);
        _ ->
            ok
    end,
    case State#state.sample_size > State#state.max_sample_size of
        true ->
            {noreply, State#state{population_size=PopulationSize}};
        false ->
            Acc = [Event|State#state.acc],
            SampleSize = State#state.sample_size + 1,
            {noreply, State#state{acc=Acc,
                                  sample_size=SampleSize,
                                  population_size=PopulationSize}}
    end.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

load_env(State) ->
    MaxSampleSize = max_sample_size(),
    State#state{max_sample_size=MaxSampleSize}.

max_sample_size() ->
    case application:get_env(appmeter, max_sample_size) of
        {ok, V} ->
            V;
        _ ->
            100
    end.
