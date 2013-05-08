-module(appmeter_event_mgr).

-export([start_link/0,
         sync_notify/2
        ]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

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

sync_notify(Pid, Event) ->
    gen_event:sync_notify(Pid, Event).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

add_handler(Pid, {Id, Info}) ->
    Module = proplists:get_value(module, Info),
    Args = proplists:get_value(args, Info, []),
    gen_event:add_handler(Pid, {Module, Id}, Args).
