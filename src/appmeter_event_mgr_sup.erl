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
