-module(callback_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).
-export([enable_node/1, timestamp/0]).
-export([add_key/2, find_key/1]).

-record(auth_key, {key :: binary(),
		   expired_at :: integer()
		  }).

%% ./erl_call -c callback -v -a "callback_app enable_node ['callback@127.0.0.1']" -n 'callback2@127.0.0.1'

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    Node = node(),
    case application:get_env(callback, admin_node) of
	{ok, N} when N==Node -> start_mnesia();
	_ -> ok
    end,
    start_http(),
    callback_sup:start_link().

stop(_State) ->
    ok.

start_mnesia() ->
    mnesia:create_table(auth_key, [{type, set},
				   {record_name, auth_key},
				   {attributes, record_info(fields, auth_key) }]).

add_key(Key, ExpiredAt) ->
    mnesia:transaction(fun() ->
			       mnesia:write(#auth_key{key=Key, expired_at=ExpiredAt})
		       end),
    Expired = mnesia:dirty_select(auth_key, [{
					       #auth_key{expired_at='$1', _='_'},
					       [{'<', '$1', {const, timestamp()} }],
					       ['$_']
					     }]),
    [mnesia:dirty_delete_object(E) || E <- Expired],
    ok.

find_key(Key) ->
    case mnesia:dirty_read({auth_key, Key}) of
	[#auth_key{expired_at=At}] ->
	    At;
	_ -> fail
    end.

enable_node(AdminNode) ->
    case net_kernel:connect_node(AdminNode) of
	true ->
	    {ok, _} = mnesia:change_config(extra_db_nodes, [node() | nodes()]),
	    mnesia:add_table_copy(auth_key, node(), ram_copies),
	    ok;
	ignored -> ignored
    end.

start_http() ->
    Dispatch = cowboy_router:compile([
				      {'_', [
					     {"/callback", callback_handler, []}
					    ]}
				     ]),
    cowboy:start_http(cowboy_http_listener,
		      100,
		      [{port, application:get_env(callback, http_port, 8080)}],
		      [{env, [{dispatch, Dispatch}] }] ).

timestamp() ->
    {Mega, Seconds, _Micro} = os:timestamp(),
    Mega*1000000 + Seconds.
