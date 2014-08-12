-module(test).
-export([main/0]).

createcars([I|T]) ->
	Pid = car:start(I),
	[Pid|createcars(T)];
createcars([]) -> [].

loop() ->
	receive
		{CarNumber, CarPosition} ->
			io:format("Car ~p now at ~p~n", [CarNumber, CarPosition]),
			loop()
	end.

main() ->
	{ok, Default} = file:consult("../priv/cars.dat"),
	createcars(Default),
	loop().
