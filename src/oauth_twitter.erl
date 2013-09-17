-module (oauth_twitter).
-export ([start/1, start/2,
		get_request_token/1,
		authorize_url/1,
		get_access_token/2,
		get/2]).

start(Name, Args) ->
	Key = utils:get_value(<<"consumer_key">>, Args, <<>>),
	Secret = utils:get_value(<<"consumer_secret">>, Args, <<>>),
	Consumer = {erlang:binary_to_list(Key), 
				erlang:binary_to_list(Secret),
				hmac_sha1},
	oauth_client:start(Name, Consumer).

start(Consumer) ->
  	oauth_client:start(Consumer).

 get_request_token({_, <<"oauth^", Package/binary>>}=Client) ->
  	URL = "https://twitter.com/oauth/request_token",
  	% @TODO: Get hostname and port from application's ENV
  	% @TODO: If the OAUTH client is registered, then maybe we can use
  	% its name as part of the callback url, so that we don't have to
  	% deal with cookie and stuffs.
  	Callback = "http://127.0.0.1:8080/oauth/twitter/" ++ erlang:binary_to_list(Package),
  	oauth_client:get_request_token(Client, URL, [{"oauth_callback", Callback}]).

authorize_url(Token) ->
  	oauth:uri("https://twitter.com/oauth/authorize", [{"oauth_token", Token}]).

get_access_token(Client, Verifier) ->
  	URL = "https://twitter.com/oauth/access_token",
  	oauth_client:get_access_token(Client, URL, [{"oauth_verifier", Verifier}]).

get(Client, Params) ->
	URL = erlang:binary_to_list(utils:get_value(<<"url">>, Params, <<"">>)),

	ParamsList = lists:foldl(
		fun({<<"headers">>, _Headers}, Accumulator) -> 
				Accumulator;
			({<<"url">>, _Url}, Accumulator) ->
				Accumulator;
			({Key, Value}, Accumulator) ->
				[{erlang:binary_to_list(Key), erlang:binary_to_list(Value)} | Accumulator]
		end, [], Params),

	oauth_client:get(Client, URL, ParamsList).