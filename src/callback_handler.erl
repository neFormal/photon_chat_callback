-module(callback_handler).

-export([init/3, terminate/3, allowed_methods/2]).
-export([content_types_provided/2, get_response/2]).


init({tcp, http}, _, _) -> {upgrade, protocol, cowboy_rest}.
terminate(_, _, _) -> ok.
allowed_methods(Req, State) -> {[<<"GET">>], Req, State}.
content_types_provided(Req, State) -> {[ {<<"text/html">>, get_response} ], Req, State}.


get_response(Req, State) ->
    {Vals, Req2} = cowboy_req:qs_vals(Req),
    Token = proplists:get_value(<<"token">>, Vals),
    case validate(application:get_env(callback, secret, undefined), Token) of
	{true, Key, ExpiredAt} ->
	    callback_app:add_key(Key, ExpiredAt),
	    {"{ResultCode: 1, Message: \"ok\"}", Req2, State};
	false ->
	    {"{ResultCode: 2, Message: \"fail\"}", Req2, State}
    end.

validate(_, undefined) -> false;
validate(Secret, Token) ->
    try
	Key = crypto:hash(md5, Secret),
	Token2 = lists:map(fun(32) -> $+; (T) -> T end, binary_to_list(Token)),
	
	Decoded = crypto:block_decrypt(aes_cbc128, Key, <<0:(16*8)>>, base64:decode(Token2)),
	[Splitted | _] = binary:split(Decoded, [<<0>>], [trim]),
	Data = binary_to_list(Splitted),
	
	Timestamp = list_to_integer(string:substr(Data, length(Data)-9)),
	AuthKey = string:substr(Data, 1, length(Data)-10),
	
	NewKey = (Timestamp >= callback_app:timestamp()) and (callback_app:find_key(AuthKey) == fail),
	
	if NewKey == true -> {true, AuthKey, Timestamp};
	   NewKey == false -> NewKey
	end
    catch _Err:_Reason ->
	    io:format("error: ~p~n ~p~n", [_Err, _Reason]),
	    false
    end.
