-module(spas_app).
-author ('esente@gmail.com').

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
	spas_store:init(), % Start the store first before any incoming request.

	Port = 8080,
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/oauth/:provider/:package", oauth_handler, []},
			% {"/oauth2", oauth2_handler, []},
			% {"/admin", admin_handler, []},
			{"/bundle/:bid", bundle_handler, []},
			{"/:bid", bundle_handler, []}
		]}
	]),
	{ok, _} = cowboy:start_http(http, 100, [{port, Port}], [
		{env, [{dispatch, Dispatch}]}
	]),

	io:format("~nSPAS is running on port ~p~n", [Port]),

    spas_sup:start_link().

stop(_State) ->
    ok.
