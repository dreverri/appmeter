%% -------------------------------------------------------------------
%%
%% Copyright (c) 2007-2013 Basho Technologies, Inc.  All Rights Reserved.
%%
%% This file is provided to you under the Apache License,
%% Version 2.0 (the "License"); you may not use this file
%% except in compliance with the License.  You may obtain
%% a copy of the License at
%%
%%   http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing,
%% software distributed under the License is distributed on an
%% "AS IS" BASIS, WITHOUT WARRANTIES OR CONDITIONS OF ANY
%% KIND, either express or implied.  See the License for the
%% specific language governing permissions and limitations
%% under the License.
%%
%% -------------------------------------------------------------------

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
