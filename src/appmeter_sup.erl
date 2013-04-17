-module(appmeter_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    EventMgrSup = supervisor_spec(appmeter_event_mgr_sup),
    ProxySup = supervisor_spec(appmeter_proxy_sup),
    {ok, {{one_for_one, 5, 10}, [EventMgrSup, ProxySup]}}.

supervisor_spec(M) ->
    {M, {M, start_link, []}, permanent, 5000, supervisor, [M]}.
