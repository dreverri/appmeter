-module(appmeter_event_mgr).

-export([start_link/0,
         notify/2
        ]).

start_link() ->
    case gen_event:start_link() of
        {ok, Pid} ->
            case application:get_env(appmeter, handlers) of
                {ok, Handlers} ->
                    [add_handler(Pid, Handler) || Handler <- Handlers],
                    {ok, Pid};
                _ ->
                    {ok, Pid}
            end;
        Error ->
            Error
    end.

add_handler(Pid, {Id, Info}) ->
    Module = proplists:get_value(module, Info),
    Args = proplists:get_value(args, Info, []),
    gen_event:add_handler(Pid, {Module, Id}, Args).

notify(Pid, Event) ->
    gen_event:notify(Pid, Event).
