-module (utils).
-author ('esente@gmail.com').

-export ([get_value/3, append_query/3, get_url/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

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