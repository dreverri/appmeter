-module(appmeter_proxy).
-behaviour(gen_server).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0,
         notify/2,
         reload/1
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

-record(state, {event_mgr,
                flush_interval,
                max_sample_size,
                timer,
                sample_size=0,
                population_size=0,
                acc=[]
               }).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link(?MODULE, [], []).

notify(Pid, Event) ->
    gen_server:cast(Pid, {notify, Event}).

reload(Pid) ->
    gen_server:cast(Pid, reload).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(_) ->
    random:seed(now()),
    EventMgr = event_mgr(),
    State = load_env(#state{event_mgr=EventMgr}),
    {ok, State}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(reload, State) ->
    State1 = load_env(State),
    {noreply, State1};

handle_cast({notify, Event}, State) ->
    PopulationSize = State#state.population_size + 1,
    case PopulationSize of
        1 ->
            erlang:send_after(State#state.flush_interval, self(), flush);
        _ ->
            ok
    end,
    case State#state.sample_size >= State#state.max_sample_size of
        true ->
            {noreply, State#state{population_size=PopulationSize}};
        false ->
            Acc = [Event|State#state.acc],
            SampleSize = State#state.sample_size + 1,
            {noreply, State#state{acc=Acc,
                                  sample_size=SampleSize,
                                  population_size=PopulationSize}}
    end.

handle_info(flush, State) ->
    State1 = flush(State),
    {noreply, State1};

handle_info({'DOWN', _, _, Pid, _}, State=#state{event_mgr=Pid}) ->
    NewEventMgr = event_mgr(),
    {noreply, State#state{event_mgr=NewEventMgr}}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

event_mgr() ->
    Pid = appmeter_event_mgr_sup:get_event_mgr(),
    monitor(process, Pid),
    Pid.

load_env(State) ->
    FlushInterval = flush_interval(),
    MaxSampleSize = max_sample_size(),
    State#state{flush_interval=FlushInterval, max_sample_size=MaxSampleSize}.

flush_interval() ->
    case application:get_env(appmeter, flush_interval) of
        {ok, V} ->
            V;
        _ ->
            100
    end.

max_sample_size() ->
    case application:get_env(appmeter, max_sample_size) of
        {ok, V} ->
            V;
        _ ->
            100
    end.

flush(State) ->
    %% NOTE: noproc errors are ignored by gen_event in the case where
    %% the event_mgr has crashed. The proxy will eventually grab a new
    %% event_mgr Pid wen it processes the 'DOWN' message.
    case State#state.acc of
        [] ->
            State#state{acc=[], sample_size=0, population_size=0};
        Acc ->
            EventMgr = State#state.event_mgr,
            SampleRate = State#state.sample_size / State#state.population_size,
            appmeter_event_mgr:notify(EventMgr, {lists:append(Acc), SampleRate}),
            State#state{acc=[], sample_size=0, population_size=0}
    end.
