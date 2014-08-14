-module(erTest_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	% {ok, Default} = file:consult("./priv/cars.dat"),
	{ok, Default} = file:consult(filename:join(code:priv_dir(erTest), "cars.dat")),
	print_info(Default),
	{ok,{{one_for_one,5,10}, lists:map(fun view/1, Default)}}.

view(I) ->
	{{number, CarNumber}, _, _} = I,
	{CarNumber, {erTest_car, start_link, [I]}, permanent, 2000, worker, [erTest_car]}.

car_numbers(I) ->
	{{number, CarNumber}, _, _} = I,
	io:format("~p~n", [CarNumber]).

print_info(Numbers) ->
	io:format("Loaded data about cars~n"),
	lists:map(fun car_numbers/1, Numbers),
	io:format("You can use erTest_car:car_start(Number) to start car~n"),
	io:format("You can use erTest_car:car_stop(Number) to stop car~n"),
	io:format("You can use erTest_car:car_position(Number) to know current car position~n").