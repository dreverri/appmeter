-module(appmeter_handler_test).

-include_lib("eunit/include/eunit.hrl").

rountrip_test() ->
    application:load(appmeter),
    application:set_env(appmeter, flush_interval, 10),
    application:set_env(appmeter, handlers, [{appmeter_handler_dummy, [self()]}]),
    application:start(appmeter),
    {ok, Pid} = appmeter:proxy(),
    appmeter_proxy:notify(Pid, [{count, "foo", 1}]),
    receive
        {Stats, SampleRate} ->
            ?assertEqual([{count, "foo", 1}], Stats),
            ?assertEqual(1.0, SampleRate)
    end.
