%% Abstract layer to handle key-to-pid mapping,
%% whether using ETS or Mnesia.
-module (spas_store).
-author ('esente@gmail.com').

-export ([init/0, insert/2, delete/1, lookup/1]).

-define (TABLE_ID, ?MODULE).

% -record (key_to_pid, {key, pid}).

init() ->
	ets:new(?TABLE_ID, [public, named_table]),
	ok.
	% mnesia:start(),
	% mnesia:create_table(key_to_pid, [{index, [pid]},
	% 					{attributes, record_info(fields, key_to_pid)}]).
	

insert(Key, Pid) ->
	% The default structure of ETS is set, of which the first element
	% of the tuple is key.
	ets:insert(?TABLE_ID, {Key, Pid}).

	% mnesia:dirty_write(#key_to_pid{key=Key, pid=Pid}).

lookup(Key) ->
	case ets:lookup(?TABLE_ID, Key) of
		% ets:lookup/2 returns list of all tuples having the Key.
		[{Key, Pid}] -> {ok, Pid};
		[]			-> {error, not_found} 
	end.

	% case mnesia:dirty_read(key_to_pid, Key) of
	% 	[{key_to_pid, Key, Pid}] ->
	% 		% The Pid returned may be dead being killed in other node.
	% 		case is_pid_alive(Pid) of
	% 			true -> {ok, Pid};
	% 			false -> {error, not_found}
	% 			% TODO: The stale pids are left in Mnesia until deleted
	% 			% or overwritten by new value. How to clean them out in
	% 			% an efficient way.
	% 		end;
	% 	[]	-> 
	% 		{error, not_found}
	% end.

delete(Pid) ->
	% Deleting should be an idempotent operation - do it over and over
	% the same result.

	ets:match_delete(?TABLE_ID, {'_', Pid}).

	% case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
	% 	[#key_to_pid{} = Record] ->
	% 		mnesia:dirty_delete_object(Record);
	% 	_ ->
	% 		ok
	% end.

% is_pid_alive(Pid) when node(Pid) =:= node() ->
% 	is_process_alive(Pid);
% is_pid_alive(Pid) ->
% 	lists:member(node(Pid), nodes()) andalso
% 	(rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).