-module(erTest_app).
-behaviour(application).
 
-export([start/2, stop/1]).

dispatch_rules() ->
    cowboy_router:compile([
        {'_', [
            {"/websocket", erTest_handler, []},
            {"/", cowboy_static, {priv_file, erTest, "index.html"}},
            {'_', notfound_handler, []}
        ]}
    ]).

start(_Type, _Args) ->
    Dispatch = dispatch_rules(),
    Port = 8008,
    {ok, _} = cowboy:start_http(http_listener, 100,
        [{port, Port}],
        [{env, [{dispatch, Dispatch}]}]
    ),
	erTest_sup:start_link().
 
stop(_State) ->
  exit(whereis(erTest_sup), shutdown).