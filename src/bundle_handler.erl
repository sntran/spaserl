-module (bundle_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([bundle_json/2]).

init(_Transport, _Req, []) ->
	% For the random number generator:
	{X, Y, Z} = now(),
	random:seed(X, Y, Z),
	{upgrade, protocol, cowboy_rest}.

allowed_methods(Req, State) ->
	{[<<"GET">>], Req, State}.

content_types_provided(Req, State) ->
	{[
		{<<"application/json">>, bundle_json}
	], Req, State}.

resource_exists(Req, _State) ->
	case cowboy_req:binding(bid, Req) of
		{undefined, Req2} ->
			{false, Req2, index};
		{BundleID, Req2} ->
			% TODO: Check if the bundle definition exists
			case spas:lookup(BundleID) of
				{ok, Cache} -> {true, Req2, Cache};
				{error, not_found} -> {false, Req2, BundleID}
			end
	end.

bundle_json(Req, Cache) ->
	{Cache, Req, Cache}.


%% PRIVATE %%
% full_path(BundleID) ->
% 	Priv = code:priv_dir(spas),
% 	filename:join([Priv, "bundles", BundleID]).

% bundle_exists(BundleID) ->
% 	case file:read_file_info(full_path(BundleID)) of
% 		{ok, _Info} -> true;
% 		{error, _Reason} -> false
% 	end.