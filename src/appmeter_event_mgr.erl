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
                    [add_handler(Pid, HandlerSpec) || HandlerSpec <- Handlers],
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

add_handler(Pid, {Handler, Args}) ->
    gen_event:add_handler(Pid, Handler, Args).
