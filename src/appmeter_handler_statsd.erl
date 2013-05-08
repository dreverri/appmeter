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

-module(appmeter_handler_statsd).

-behavior(gen_event).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {pid}).

init(_) ->
    case statsd_client:start_link() of
        {ok, Pid} ->
            {ok, #state{pid=Pid}};
        {error, Reason} ->
            {stop, Reason}
    end.

handle_event({Stats, SampleRate}, State) ->
    State1 = handle_stats(Stats, SampleRate, State),
    {ok, State1}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

handle_stats(Stats, SampleRate, State) ->
    Metrics = stats_to_metrics(Stats, SampleRate),
    statsd_client:metrics(State#state.pid, Metrics),
    State.

stats_to_metrics(Stats, SampleRate) ->
    Fun = fun(Stat, Metrics) ->
            case stat_to_metric(Stat, SampleRate) of
                undefined ->
                    Metrics;
                Metric ->
                    [Metric|Metrics]
            end
    end,
    lists:foldl(Fun, [], Stats).

stat_to_metric({count, Name, Value}, SampleRate) ->
    {count, Name, Value, SampleRate};

stat_to_metric({measure, Name, Value}, SampleRate) ->
    {time, Name, Value, SampleRate};

stat_to_metric({gauge, Name, Value}, _SampleRate) ->
    {gauge, Name, Value};

stat_to_metric(_, _) ->
    undefined.
