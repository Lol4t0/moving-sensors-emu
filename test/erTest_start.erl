-module(erTest_start).

-export([start_stuff/0]).

start_stuff() ->
	lager:start(),
	application:start(erTest).