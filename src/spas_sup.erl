-module(spas_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define (SERVER, ?MODULE).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	CacheManager = {cache_manager, {cache_manager, start_link, []},
					permanent, 2000, supervisor, [cache]},

	{ok, Cwd} = file:get_cwd(),
	BundlesPath = filename:join([Cwd, "priv", "bundles"]),

	Watcher = {bundle_watcher, {bundle_watcher, start_link, [BundlesPath, 5000]},
				permanent, 2000, worker, [bundle_watcher]},

	Children = [CacheManager, Watcher],
	RestartStrategy = {one_for_one, 4, 3600},
	{ok, {RestartStrategy, Children}}.

