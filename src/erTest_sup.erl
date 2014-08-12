% -module(erTest_test).
% -export([main/0]).

% createcars([I|T]) ->
% 	Pid = car:start(I),
% 	[Pid|createcars(T)];
% createcars([]) -> [].

% loop() ->
% 	receive
% 		{CarNumber, CarPosition} ->
% 			io:format("Car ~p now at ~p~n", [CarNumber, CarPosition]),
% 			loop()
% 	end.

% main() ->
% 	{ok, Default} = file:consult("../priv/cars.dat"),
% 	createcars(Default),
% 	loop().

-module(erTest_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	{ok, Default} = file:consult("./priv/cars.dat"),
	{ok,{{one_for_one,5,10}, lists:map(fun view/1, Default)}}.

view(I) ->
	{{number, CarNumber}, {route, _CarRoute}, {speed, _CarSpeed}} = I,
	{CarNumber, {erTest_car, start, [I]}, permanent, 2000, worker, [erTest_car]}.
