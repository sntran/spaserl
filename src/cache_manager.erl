-module (cache_manager).
-behaviour (supervisor).

%% API
-export([start_link/0, start_child/2]).

%% Supervisor callbacks
-export([init/1]).

-define (SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

start_child(Value, LeaseTime) ->
	supervisor:start_child(?SERVER, [Value, LeaseTime]).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
	Worker = {cache, {cache, start_link, []},
				temporary, brutal_kill, worker, [cache]},
	Children = [Worker],
	RestartStrategy = {simple_one_for_one, 0, 1},
    {ok, { RestartStrategy, Children} }.