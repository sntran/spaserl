-module (bundle_handler).

-export([init/3]).
-export([allowed_methods/2]).
-export([content_types_provided/2]).
-export([resource_exists/2]).

-export([bundle_json/2]).

-define (GET, <<"spashttp.request">>).

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
	Result = [fulfill(Name, Definition, BundleID) 
		|| {Name, Definition} <- Packages],
	{Result, Req, BundleID}.


%% PRIVATE %%
fulfill(Name, Definition, BundleID) ->
	Key = <<BundleID/binary, "^", Name/binary>>,
	case spas:lookup(Key) of
		{error, not_found} ->
			perform_request(Key, Definition);
		{ok, Value} -> Value
	end.

perform_request(Key, Definition) ->
	[Resource, Params, LeaseTime, Timeout] = get_values(Definition),
	Result = execute_request(Resource, Params, Timeout),
	spas:insert(Key, Result, LeaseTime),
	Result.

execute_request(?GET, Params, Timeout) ->
	Headers = get_value(<<"headers">>, Params, []),
	FullUrl = get_url(Params),
	UrlString = binary:bin_to_list(FullUrl),

	{ok, {{_Version, 200, _ReasonPhrase}, _H, Body}} = 
		httpc:request(get, {UrlString, to_string_headers(Headers)}, 
							[{timeout, Timeout}, {ssl,[{verify,0}]}], []),
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

append_query(Url, Key, Value) ->
	<<Url/binary, "&", Key/binary, "=", Value/binary>>.

to_string_headers([]) -> [];
to_string_headers([{Key, Value} | Rest]) ->
	Header = {binary:bin_to_list(Key), binary:bin_to_list(Value)},
	[Header | to_string_headers(Rest)].