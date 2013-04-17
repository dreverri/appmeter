-module(appmeter_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_proxy/0,
         reload/0
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_proxy() ->
    supervisor:start_child(?MODULE, []).

reload() ->
    Children = supervisor:which_children(?MODULE),
    [appmeter_proxy:reload(Pid) || {_, Pid, _, _} <- Children],
    ok.

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ProxySpec = {undefined, {appmeter_proxy, start_link, []}, temporary,
                 5000, worker, [appmeter_proxy]},
    {ok, {{simple_one_for_one, 5, 10}, [ProxySpec]}}.
