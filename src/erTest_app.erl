-module(erTest_app).
-behaviour(application).
 
-export([start/2, stop/1]).

start(_Type, _Args) ->
	erTest_msup:start_link().
 
stop(_State) ->
  exit(whereis(erTest_sup), shutdown).