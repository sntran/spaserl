-module (bundle_handler).
-author ('esente@gmail.com').

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([bundle_json/2]).

-include ("spas.hrl").

%% ==================================================================
%% REST' state machine
%% ==================================================================
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
			% Check if the bundle definition exists
			case spas:lookup(BundleID) of
				{ok, _Def} -> {true, Req2, BundleID};
				{error, not_found} -> {false, Req2, BundleID}
			end
	end.

bundle_json(Req, BundleID) ->
	% Maintain session
	{ExistingId, Req1} = cowboy_req:cookie(<<"session_id">>, Req),
	SessionId = case ExistingId of
		undefined -> new_id();
		_ -> ExistingId
	end,
	Req2 = cowboy_req:set_resp_cookie(
			<<"session_id">>, SessionId, [], Req1),

	{ok, Binary} = spas:lookup(BundleID),
	Packages = jsx:decode(Binary),
	Result = join_json(Packages, BundleID),
	Response = jsx:minify(<<"{", Result/binary, "}">>),
	{Response, Req2, BundleID}.

%% ==================================================================
%% Internal Funtions
%% ==================================================================

%%--------------------------------------------------------------------
%% @doc Concat the result as JSON after fulfilling.
%%
%% If multiple packages, append comma into the binary.
%% @end
%%--------------------------------------------------------------------
% -spec join_json(list(), bundle_id()) -> binary().
join_json([], _BundleID) -> <<>>;
join_json([{Name, Definition}], BundleID) -> 
	fulfill(Name, Definition, BundleID);
join_json([{Name, Definition} | T], BundleID) -> 
	Data = fulfill(Name, Definition, BundleID),
	Next = join_json(T, BundleID),
	<<Data/binary, ",", Next/binary>> .

%%--------------------------------------------------------------------
%% @doc Ensure returning requested data, either fresh or from cache.
%%
%% This function fulfill a package in the bundle. It will look up the
%% cache for any, and perform request if need.
%% @end
%%--------------------------------------------------------------------
% -spec fulfill(binary(), list(), bundle_id()) -> binary().
fulfill(Name, Definition, BundleID) ->
	PackageKey = ?packageKey(Name, BundleID),
	Bin = case spas:lookup(PackageKey) of
		{error, not_found} ->
			case perform_request(PackageKey, get_values(Definition)) of
				{ok, Value} -> Value;
				{authorize, Url} ->
					<<"{\"authorize_url\": \"", Url/binary, "\"}">>;
				{error, Error} ->
					<<"{\"error\": \"", Error/binary, "\"}">>
			end;
		{ok, Value} -> Value
	end,
	<<"\"", Name/binary, "\":", Bin/binary>>.

%%--------------------------------------------------------------------
%% @doc Performs request to remote server based on bundle's definition
%%
%% This function parses the bundle's definition for resource, params,
%% lease time, request's timeout, filter, cleanup, etc... It then
%% execute the request, getting data, and cache them.
%% @end
%%--------------------------------------------------------------------
% -spec perform_request(binary(), list()) -> binary().
perform_request(PackageKey, [Resource, Params, LeaseTime, Timeout, _Filters, Auth]) ->
	case execute_request(PackageKey, Resource, Params, Timeout, Auth) of
		{ok, Result} ->
			% apply_filter(Filters, jsx:decode(Result)),
			spas:insert(PackageKey, Result, LeaseTime),
			{ok, Result};
		Other -> Other
	end.

%%--------------------------------------------------------------------
%% @doc Retrieves response's body from remote server.
%%
%% This function constructs the full URL from the params, then uses a
%% HTTP client to retrieve response from remote server.
%% @end
%%--------------------------------------------------------------------
% -spec perform_request(binary(), list(), integer()) -> binary().
execute_request(_PackageKey, ?GET, Params, Timeout, []) ->
	Headers = utils:get_value(<<"headers">>, Params, []),
	FullUrl = utils:get_url(Params),
	UrlString = binary:bin_to_list(FullUrl),

	{ok, {{_Version, 200, _ReasonPhrase}, _H, Body}} = 
		httpc:request(get, {UrlString, to_string_headers(Headers)}, 
							[{timeout, Timeout}, {ssl,[{verify,0}]}], [{body_format, binary}]),
	{ok, Body};

execute_request(PackageKey, ?GET, Params, _Timeout, 
				[{<<"type">>, <<"oauth">>}, {<<"provider">>, Provider} | Args]) ->
	OAUTH_Mod = erlang:binary_to_atom(<<"oauth_", Provider/binary>>, utf8),
	OAUTHClientRef = {global, PackageKey},

	case OAUTH_Mod:start(OAUTHClientRef, Args) of
		{ok, _Pid} -> 
			{ok, Token} = OAUTH_Mod:get_request_token(OAUTHClientRef),
			AuthorizeURL = erlang:list_to_binary(OAUTH_Mod:authorize_url(Token)),
			{authorize, AuthorizeURL};
		{error, {already_started, _}} ->
			% Either already having access token, or not
			{ok, _Headers, Body} = OAUTH_Mod:get(OAUTHClientRef, Params),
			{ok, Body}
	end;

execute_request(_Key, _Unknown, _Params, _Timeout, _Auth) ->
	% No known resource, just return an empty body, or an error message.
	{error, <<"No Known Method">>}.

get_values(Definition) ->
	Resource = utils:get_value(<<"resource">>, Definition, ?GET),
	Params = utils:get_value(<<"params">>, Definition, []),
	LeaseTime = utils:get_value(<<"cacheduration">>, Definition, infinity),
	Timeout = utils:get_value(<<"timeout">>, Definition, infinity),
	Filters = utils:get_value(<<"filter">>, Definition, []),
	Auth = utils:get_value(<<"auth">>, Definition, []),
	[Resource, Params, LeaseTime, Timeout, Filters, Auth].

to_string_headers([]) -> [];
to_string_headers([{Key, Value} | Rest]) ->
	Header = {binary:bin_to_list(Key), binary:bin_to_list(Value)},
	[Header | to_string_headers(Rest)].

-spec new_id() -> binary().
new_id() ->
	Data = term_to_binary([make_ref(), now(), random:uniform()]),
	Sha = binary:decode_unsigned(crypto:hash(sha, Data)),
	list_to_binary(lists:flatten(io_lib:format("~40.16.0b", [Sha]))).