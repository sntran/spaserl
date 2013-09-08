-module(spas_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	spas_store:init(), % Start the store first before any incoming request.

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/bundle/:bid", bundle_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, 8080}], [
		{env, [{dispatch, Dispatch}]}
	]),

    spas_sup:start_link().

stop(_State) ->
    ok.
