-module (spas).

-export ([start/0]).
%% API %%
-export ([insert/2, insert/3, lookup/1, delete/1]).

start() ->
	% Entry point, starting all necessary applications.
	ok = application:start(crypto),
	ok = application:start(ranch),
	ok = application:start(cowboy), % For REST interface
	ok = inets:start(), % For making request to services
	ok = ssl:start(),
	ok = application:start(spas).

%% ==================================================================
%% MAIN APIs
%% ==================================================================
insert(Key, Value) ->
	insert(Key, Value, infinity).
insert(Key, Value, LeaseTime) ->
	case spas_store:lookup(Key) of
		% check if we have a cache already.
		{ok, Pid} ->
			% Then just replace its value.
			cache:replace(Pid, Value);
		{error, _} ->
			% Else create a new cache and store the value.
			{ok, Pid} = cache:create(Value, LeaseTime),
			spas_store:insert(Key, Pid)
	end.

lookup(Key) ->
	try
		{ok, Pid} = spas_store:lookup(Key),
		{ok, _Value} = cache:fetch(Pid)
	catch
		_Class:_Exception ->
			% We have not cached it.
		    {error, not_found}
	end.

delete(Key) ->
	case spas_store:lookup(Key) of
		{ok, Pid} ->
			cache:delete(Pid);
		{error, _Reason} ->
			ok
	end.