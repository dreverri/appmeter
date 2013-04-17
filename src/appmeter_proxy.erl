-module(appmeter_proxy).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/2,
         set_sample_rate/3,
         notify/2
        ]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3
        ]).

-record(state, {sample_size, population_size, event_mgr, count=0}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(SampleSize, PopulationSize) ->
    gen_server:start_link(?MODULE, {SampleSize, PopulationSize}, []).

set_sample_rate(Pid, SampleSize, PopulationSize) ->
    gen_server:cast(Pid, {sample_rate, SampleSize, PopulationSize}).

notify(Pid, Events) ->
    gen_server:cast(Pid, {notify, Events}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init({SampleSize, PopulationSize}) ->
    self() ! init,
    {ok, #state{sample_size=SampleSize,
                population_size=PopulationSize}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({sample_rate, SampleSize, PopulationSize}, State) ->
    %% TODO: should the count be reset? should current sample be completed
    %% before changing the sampling rate?
    {noreply, State#state{sample_size=SampleSize,
                          population_size=PopulationSize}};

handle_cast({notify, Events}, State) ->
    maybe_notify(State, Events),
    Count = State#state.count,
    case Count > State#state.population_size of
        true ->
            {noreply, State#state{count=0}};
        false ->
            {noreply, State#state{count=Count+1}}
    end.

handle_info(init, State) ->
    EventMgr = appmeter:event_mgr(),
    monitor(process, EventMgr),
    {noreply, State#state{event_mgr=EventMgr}};

handle_info({'DOWN', _, _, Pid, _}, State=#state{event_mgr=Pid}) ->
    NewEventMgr = appmeter:event_mgr(),
    monitor(process, NewEventMgr),
    {noreply, State#state{event_mgr=NewEventMgr}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

maybe_notify(State, Events) ->
    case State#state.count =< State#state.sample_size of
        true ->
            %% NOTE: noproc errors are ignored by gen_event in the case where
            %% the event_mgr has crashed. The proxy will eventually grab a new
            %% event_mgr Pid wen it processes the 'DOWN' message.
            SampleRate = State#state.sample_size / State#state.population_size,
            appmeter_event_mgr:notify(State#state.event_mgr, {Events, SampleRate});
        false ->
            ok
    end.
