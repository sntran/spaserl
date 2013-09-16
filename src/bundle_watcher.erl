-module (bundle_watcher).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-include ("spas.hrl").

-record (state, {dir, interval, timer, changes=[]}).

start_link(Dir, Interval) ->
    gen_server:start_link(?MODULE, [Dir, Interval], []).

%% Callbacks %%
init([Dir, Interval]) ->
	process_flag(trap_exit, true),

	{Files, Refs} = inspect(Dir),
	update_bundle_files(Files),
	{ok, #state{dir = Dir, interval = Interval, changes = Refs,
				timer = erlang:start_timer(Interval, self(), reload)}}.

handle_call(_Event, _From, State) ->
    {noreply, State}.

handle_cast(_Event, State) ->
    {noreply, State}.

handle_info({timeout, Ref, reload}, State=#state{dir=Dir, 
											interval = Interval, 
											timer=Ref, 
											changes=Changes}) ->
	{DiffFiles, NewRefs} = inspect(Dir, Changes),
	case DiffFiles of
		[] -> ok;
		_ -> update_bundle_files(DiffFiles)
	end,
	Timer = erlang:start_timer(Interval, self(), reload),
	{noreply, State#state{timer = Timer,
                      changes = NewRefs}};
	
handle_info(_Event, State) ->
    {noreply, State}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

terminate(_Reason, _State) ->
    ok.

%% ==================================================================
%% Internal Funtions
%% ==================================================================
update_bundle_files([]) -> ok;
update_bundle_files([File | Rest]) ->
	Bundle = binary:list_to_bin(filename:basename(File, ".json")),
	{ok, Bin} = file:read_file(File),
	spas:insert(Bundle, Bin),
	diff_package(jsx:decode(Bin), Bundle),
	update_bundle_files(Rest).

find_bundle_files(Directory) ->
    {ok, Files} = file:list_dir(Directory),
    [filename:join(Directory, Name) || Name <- Files,
        ".json" =:= filename:extension(Name)].

%%--------------------------------------------------------------------
%% @doc Remove the cache for package whose definition changes
%%
%% During a watch, if any package in bundle's definition changes,
%% this function removes the corresponding cache so the next request 
%% will use the new definition.
%% @end
%%--------------------------------------------------------------------
diff_package([], _) -> ok;
diff_package([{Package, _Definition} | Rest], Bundle) ->
	Key = ?packageKey(Package, Bundle),
	case spas:lookup(Key) of
		{ok, _Value} -> spas:delete(Key);
		{error, not_found} -> ok
	end,
	diff_package(Rest, Bundle).

%%--------------------------------------------------------------------
%% @doc Check for directory's change since last check.
%%
%% This function compares the hash of previous changes if any, on each
%% file in the directory, and return the list of files that change.
%% @end
%%--------------------------------------------------------------------
% -spec inspect(binary(), list()) -> {[file(), [{file(), hash()}]]}.
inspect(Dir) ->
	inspect(Dir, []).
inspect(Dir, Refs) ->
	Files = find_bundle_files(Dir),
	NewRefs = [begin
                {ok, Bin} = file:read_file(File),
                {File, crypto:hash(md5, Bin)}
               end || File <- Files],
    DiffFiles = diff(NewRefs, Refs),
    {DiffFiles, NewRefs}.

diff([], _Refs) -> [];
diff([{File, Ref} | Rest], Refs) ->
    case lists:keyfind(Ref, 2, Refs) of
        false -> [File | diff(Rest, Refs)];
        _ -> diff(Rest, Refs)
    end.