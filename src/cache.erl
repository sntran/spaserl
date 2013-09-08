-module (cache).
-behaviour (gen_server).

-export ([start_link/2,
			create/2,
			create/1,
			fetch/1,
			replace/2,
			delete/1]).	

-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define (SERVER, ?MODULE).
-define (DEFAULT_LEASE_TIME, (60*60*24)).

-record (state, {value, lease_time, start_time}).

start_link(Value, LeaseTime) ->
	% Not called directly; called by supervisor.
	gen_server:start_link(?MODULE, [Value, LeaseTime], []).

%% API %%
create(Value, LeaseTime) ->
	cache_manager:start_child(Value, LeaseTime).

create(Value) ->
	create(Value, ?DEFAULT_LEASE_TIME).

fetch(Pid) ->
	gen_server:call(Pid, fetch).

replace(Pid, Value) ->
	gen_server:cast(Pid, {replace, Value}).

delete(Pid) ->
	gen_server:cast(Pid, delete).

%% Callbacks %%
init([Value, LeaseTime]) ->
	StartTime = get_current_time(),
	% Store the data, and set a timeout for this cache.
	% After the lease time, a message will be sent to this process,
	% and handle_info/1 will shuts down the process.
	{ok,
	 #state{value = Value,
	 		lease_time = LeaseTime,
	 		start_time = StartTime},
	 time_left(StartTime, LeaseTime)}.

handle_call(fetch, _From, State) ->
	#state{value = Value, lease_time = LeaseTime, start_time = StartTime} = State,
	TimeLeft = time_left(StartTime, LeaseTime),
	{reply, {ok, Value}, State, TimeLeft}.

handle_cast({replace, Value}, State) ->
	#state{lease_time = LeaseTime, start_time = StartTime} = State,
	TimeLeft = time_left(StartTime, LeaseTime),
	{noreply, State#state{value=Value}, TimeLeft};

handle_cast(delete, State) ->
	{stop, normal, State}.

handle_info(timeout, State) ->
	{stop, normal, State}.

terminate(_Reason, _State) ->
	% TODO: Handle when cache crashes abnormally; need to restart it.
	spas_store:delete(self()),
	ok.

code_change(_OldVsn, State, _Extra) ->
	{ok, State}.

%% INTERNAL %%
get_current_time() ->
	Now = calendar:local_time(),
	calendar:datetime_to_gregorian_seconds(Now).

% Computes the number of milliseconds left of the lease.
time_left(_StartTime, infinity) ->
	infinity;
time_left(StartTime, LeaseTime) ->
	CurrentTime = get_current_time(),
	TimeElapsed = CurrentTime - StartTime,
	case LeaseTime - TimeElapsed of
		Time when Time =< 0 -> 0;
		Time 				-> Time * 1000
	end.