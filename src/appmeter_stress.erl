-module(appmeter_stress).

-export([run/0]).

run() ->
    application:load(appmeter),
    application:set_env(appmeter, handlers, [{statsd, [
                    {module, appmeter_handler_statsd},
                    {args, []}]}]),
    application:start(appmeter),
    Reporter = spawn(fun reporter/0),
    Runners = [spawn(fun runner/0) || _ <- lists:seq(1, 100)],
    spawn(fun() -> control([Reporter|Runners]) end).

reporter() ->
    EventMgrs = supervisor:which_children(appmeter_sup),
    [report_child(EventMgr) || EventMgr <- EventMgrs],

    Proxies = supervisor:which_children(appmeter_proxy_sup),
    case Proxies of
        [] ->
            ok;
        _ ->
            Qs = [message_queue_len(Pid) || {_, Pid, _, _} <- Proxies],
            Count = length(Qs),
            Min = lists:min(Qs),
            Max = lists:max(Qs),
            Sum = lists:sum(Qs),
            Mean = Sum / Count,
            io:format("proxies: ~p/~p/~p (min/mean/max)~n", [Min, Mean, Max])
    end,
    receive
        stop ->
            ok
    after
        1000 ->
            reporter()
    end.

report_child({Id, Pid, _, _}) ->
    Len = message_queue_len(Pid),
    io:format("~p (~p): ~p~n", [Id, Pid, Len]).

message_queue_len(Pid) ->
    {_, Len} = erlang:process_info(Pid, message_queue_len),
    Len.

runner() ->
    {ok, Pid} = appmeter:proxy(),
    runner(Pid, 0).

runner(Pid, Count) ->
    appmeter_proxy:notify(Pid, [{measure, "foo", 1}]),
    receive
        stop ->
            ok
    after
        0 ->
            runner(Pid, Count+1)
    end.

control(Pids) ->
    receive
        stop ->
            [Pid ! stop || Pid <- Pids]
    end.
