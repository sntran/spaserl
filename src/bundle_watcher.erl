-module (bundle_watcher).
-behaviour (gen_server).

-export ([start_link/2]).
-export ([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record (state, {dir, interval, timer, changes=[]}).

start_link(Dir, Interval) ->
    gen_server:start_link(?MODULE, [Dir, Interval], []).

%% Callbacks %%
init([Dir, Interval]) ->
	process_flag(trap_exit, true),

	{_Files, Refs} = inspect(Dir),
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

%% INTERNAL %%
update_bundle_files([]) -> ok;
update_bundle_files([File | Rest]) ->
	% A file can have multiple bundles.
	{ok, Bin} = file:read_file(File),
	Bundles = jsx:decode(Bin),
	[spas:insert(Name, Definition) || {Name, Definition} <- Bundles],
	update_bundle_files(Rest).

find_bundle_files(Directory) ->
    {ok, Files} = file:list_dir(Directory),
    [filename:join(Directory, Name) || Name <- Files,
        ".json" =:= filename:extension(Name)].

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