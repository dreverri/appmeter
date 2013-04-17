-module(appmeter_proxy_sup).

-behaviour(supervisor).

%% API
-export([start_link/0,
         start_proxy/0,
         set_sample_rate/2
        ]).

%% Supervisor callbacks
-export([init/1]).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

start_proxy() ->
    S = sample_size(),
    P = population_size(),
    supervisor:start_child(?MODULE, [S, P]).

set_sample_rate(SampleSize, PopulationSize) ->
    application:set_env(appmeter, sample_size, SampleSize),
    application:set_env(appmeter, population_size, PopulationSize),
    [appmeter_proxy:set_sample_rate(Pid, SampleSize, PopulationSize) ||
        {_, Pid, _, _} <- supervisor:which_children(?MODULE)].

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    ProxySpec = {undefined, {appmeter_proxy, start_link, []}, temporary,
                 5000, worker, [appmeter_proxy]},
    {ok, {{simple_one_for_one, 5, 10}, [ProxySpec]}}.

sample_size() ->
    case application:get_env(appmeter, sample_size) of
        {ok, S} when is_integer(S) ->
            S;
        _ ->
            1
    end.

population_size() ->
    case application:get_env(appmeter, population_size) of
        {ok, P} when is_integer(P) ->
            P;
        _ ->
            10
    end.
