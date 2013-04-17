-module(appmeter_event_mgr_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         get_event_mgr/0
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

get_event_mgr() ->
    Children = supervisor:which_children(?MODULE),
    EventMgrs = [Pid || {_, Pid, _, dynamic} <- Children],
    random:seed(now()),
    N = random:uniform(length(EventMgrs)),
    lists:nth(N, EventMgrs).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    C = event_mgr_count(),
    EventMgrs = [event_mgr_spec(I) || I <- lists:seq(1, C)],
    {ok, {{one_for_one, 5, 10}, EventMgrs}}.

event_mgr_count() ->
    case application:get_env(appmeter, event_mgr_count) of
        {ok, C} when is_integer(C) ->
            C;
        _ ->
            8
    end.

event_mgr_spec(I) ->
    {I, {appmeter_event_mgr, start_link, []}, permanent, 5000, worker, dynamic}.
