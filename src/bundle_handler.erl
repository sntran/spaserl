-module (bundle_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([bundle_json/2]).

-define (GET, <<"spashttp.request">>).

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
	{ok, Binary} = spas:lookup(BundleID),
	Packages = jsx:decode(Binary),
	Result = join_json(Packages, BundleID),
	Response = jsx:minify(<<"{", Result/binary, "}">>),
	{Response, Req, BundleID}.

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
	Key = <<BundleID/binary, "^", Name/binary>>,
	Bin = case spas:lookup(Key) of
		{error, not_found} ->
			perform_request(Key, Definition);
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
perform_request(Key, Definition) ->
	[Resource, Params, LeaseTime, Timeout] = get_values(Definition),
	Result = execute_request(Resource, Params, Timeout),
	spas:insert(Key, Result, LeaseTime),
	Result.

%%--------------------------------------------------------------------
%% @doc Retrieves response's body from remote server.
%%
%% This function constructs the full URL from the params, then uses a
%% HTTP client to retrieve response from remote server.
%% @end
%%--------------------------------------------------------------------
% -spec perform_request(binary(), list(), integer()) -> binary().
execute_request(?GET, Params, Timeout) ->
	Headers = get_value(<<"headers">>, Params, []),
	FullUrl = get_url(Params),
	UrlString = binary:bin_to_list(FullUrl),

	{ok, {{_Version, 200, _ReasonPhrase}, _H, Body}} = 
		httpc:request(get, {UrlString, to_string_headers(Headers)}, 
							[{timeout, Timeout}, {ssl,[{verify,0}]}], [{body_format, binary}]),
	Body;

execute_request(_Unknown, _Params, _Timeout) ->
	% No known resource, just return an empty body, or an error message.
	<<"">>.

get_values(Definition) ->
	Resource = get_value(<<"resource">>, Definition, ?GET),
	Params = get_value(<<"params">>, Definition, []),
	LeaseTime = get_value(<<"cacheduration">>, Definition, infinity),
	Timeout = get_value(<<"timeout">>, Definition, infinity),
	[Resource, Params, LeaseTime, Timeout].

get_value(Key, TupleList, Default) ->
	case lists:keyfind(Key, 1, TupleList) of
		{Key, Value} -> Value;
		false -> Default
	end.

get_url(Params) ->
	% TODO: Set default URL to this server to return an error message.
	Url = get_value(<<"url">>, Params, <<"">>),
	lists:foldl(
		fun({<<"headers">>, _Headers}, Accumulator) -> 
				Accumulator;
			({<<"url">>, _Url}, Accumulator) ->
				Accumulator;
			({Key, Value}, Accumulator) ->
				append_query(Accumulator, Key, Value)
		end, <<Url/binary, "?">>, Params).

append_query(Url, Key, Value) when is_integer(Value) ->
	<<Url/binary, "&", Key/binary, "=", Value>>;
append_query(Url, Key, Value) ->
	<<Url/binary, "&", Key/binary, "=", Value/binary>>.

to_string_headers([]) -> [];
to_string_headers([{Key, Value} | Rest]) ->
	Header = {binary:bin_to_list(Key), binary:bin_to_list(Value)},
	[Header | to_string_headers(Rest)].