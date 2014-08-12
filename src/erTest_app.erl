-module(erTest_app).
-behaviour(application).
 
-export([start/2, stop/1]).

start(_Type, _Args) ->
	case erTest_sup:start_link() of
		{ok, Pid} -> 
			{ok, Pid};
		Error ->
			Error
  end.
 
stop(_State) ->
  exit(whereis(erTest_sup), shutdown).