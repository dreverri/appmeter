-module(appmeter).

-export([proxy/0,
        measure/2,
        count/2,
        gauge/2
       ]).

proxy() ->
    appmeter_proxy_sup:start_proxy().

count(Name, Value) ->
    {count, Name, Value}.

measure(Name, Value) ->
    {measure, Name, Value}.

gauge(Name, Value) ->
    {gauge, Name, Value}.
