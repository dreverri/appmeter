-module(appmeter).

-export([event_mgr/0,
        proxy/0,
        measure/2,
        count/2,
        gauge/2
       ]).

event_mgr() ->
    appmeter_sup:random_event_mgr().

proxy() ->
    appmeter_proxy_sup:start_proxy().

count(Name, Value) ->
    {count, Name, Value}.

measure(Name, Value) ->
    {measure, Name, Value}.

gauge(Name, Value) ->
    {gauge, Name, Value}.
