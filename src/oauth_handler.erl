-module (oauth_handler).
-author ('esente@gmail.com').

% @ref: https://github.com/synrc/avz
-export([init/3]).
-export([handle/2]).
-export([terminate/3]).

init(_Transport, Req, []) ->
	{ok, Req, undefined}.

handle(Req, _State) ->
	{Status, FinalReq, State} = case cowboy_req:binding(package, Req) of
		{undefined, Req2} ->
			{false, Req2, index};
		{Package, Req2} ->
			{Provider, Req3} = cowboy_req:binding(provider, Req2),
			OAUTH_Mod = erlang:binary_to_atom(<<"oauth_", Provider/binary>>, utf8),
			OAUTHClientRef = {global, <<"oauth^", Package/binary>>},
			{_Token, Req4} = cowboy_req:qs_val(<<"oauth_token">>, Req3),
			{VerifierPIN, Req5} = cowboy_req:qs_val(<<"oauth_verifier">>, Req4),
			ok = OAUTH_Mod:get_access_token(OAUTHClientRef, erlang:binary_to_list(VerifierPIN)),
			
			% We're done here, probably look up which bundle this package belongs,
			% and redirect the client there.

			{ok, Req6} = cowboy_req:reply(200, [], 
							<<"Authorized for ", Package/binary, "!">>, Req5),

			{ok, Req6, index}
	end,
	{Status, FinalReq, State}.

terminate(_Reason, _Req, _State) ->
	ok.
