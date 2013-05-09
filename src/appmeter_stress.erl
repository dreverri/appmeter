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

-module(appmeter_stress).

-export([start/0]).

start() ->
    application:load(appmeter),
    application:set_env(appmeter, handlers, [{appmeter_handler_stats, []}]),
    application:start(appmeter),
    Runners = [spawn(fun runner/0) || _ <- lists:seq(1, 200)],
    Reporter = spawn(fun() -> reporter(Runners) end),
    spawn(fun() -> control([Reporter|Runners]) end).

reporter(Runners) ->
    io:format("Summaries~n"),
    report_event_mgrs(),
    report_proxies(),
    report_runners(Runners),
    io:format("~n"),
    receive
        stop ->
            ok
    after
        1000 ->
            reporter(Runners)
    end.

report_event_mgrs() ->
    EventMgrs = supervisor:which_children(appmeter_event_mgr_sup),
    Qs = [message_queue_len(Pid) || {_, Pid, _, _} <- EventMgrs],
    summarize("event_mgr msg_qs (lower is better)", Qs).

report_proxies() ->
    Proxies = supervisor:which_children(appmeter_proxy_sup),
    case Proxies of
        [] ->
            ok;
        _ ->
            Qs = [message_queue_len(Pid) || {_, Pid, _, _} <- Proxies],
            summarize("proxy msg_qs (lower is better)", Qs)
    end.

report_runners(Runners) ->
    Results = [collect(Runner) || Runner <- Runners],
    summarize("runner throughput (higher is better)", Results).

message_queue_len(Pid) ->
    {_, Len} = erlang:process_info(Pid, message_queue_len),
    Len.

collect(Runner) ->
    Runner ! {report, self()},
    receive
        Count ->
            Count
    end.

summarize(Name, Results) ->
    Count = length(Results),
    Min = lists:min(Results),
    Max = lists:max(Results),
    Sum = lists:sum(Results),
    Mean = Sum/Count,
    io:format("~p: ~p/~p/~p (min/mean/max)~n", [Name, Min, Mean, Max]).

runner() ->
    {ok, Pid} = appmeter:proxy(),
    runner(Pid, 0).

runner(Pid, Count) ->
    appmeter_proxy:notify(Pid, [{count, "foo", 1}]),
    receive
        stop ->
            ok;
        {report, Reporter} ->
            Reporter ! Count,
            runner(Pid, 0)
    after
        0 ->
            runner(Pid, Count+1)
    end.

control(Pids) ->
    receive
        stop ->
            [Pid ! stop || Pid <- Pids]
    end.
