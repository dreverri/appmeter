-module(appmeter_handler_dummy).

-behavior(gen_event).

-export([init/1,
         handle_event/2,
         handle_call/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

init([Pid]) ->
    {ok, Pid}.

handle_event(Event, Pid) ->
    Pid ! Event,
    {ok, Pid}.

handle_call(_Request, State) ->
    {ok, ok, State}.

handle_info(_Info, State) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.
