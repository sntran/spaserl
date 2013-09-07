-module (spas).

start() ->
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(spas).

insert(Key, Value) ->
	case spas_store:lookup(Key) of
		% check if we have a cache already.
		{ok, Pid} ->
			% Then just replace its value.
			cache:replace(Pid, Value);
		{error, _} ->
			% Else create a new cache and store the value.
			{ok, Pid} = cache:create(Value),
			spas_store:insert(Key, Pid)
	end.

lookup(Key) ->
	try
		{ok, Pid} = spas_store:lookup(Key),
		cache:fetch(Pid)
	catch
		_Class:_Exception ->
		    {error, not_found}
	end.

delete(Key) ->
	case spas_store:lookup(Key) of
		{ok, Pid} ->
			cache:delete(Pid);
		{error, _Reason} ->
			ok
	end.