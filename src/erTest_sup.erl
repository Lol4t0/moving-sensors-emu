-module(erTest_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(MAPSIZE, 100).
-define(ROUTES, 10000).
-define(CARS, 120000).

start_link() ->
	statistics(runtime),
	statistics(wall_clock),
	supervisor:start_link({local, ?SERVER}, ?MODULE, []),
	{_, Time1} = statistics(runtime),
	{_, Time2} = statistics(wall_clock),
	io:format("Data loaded for = ~p (~p) seconds~n", [Time1 div 1000, Time2 div 1000]).

init([]) ->
	% {ok, Default} = file:consult(filename:join(code:priv_dir(erTest), "cars.dat")),
	Default = generate_cars(?CARS, generate_routes(?ROUTES)),
	% print_info(Default),
	{ok,{{one_for_one,5,10}, lists:map(fun view/1, Default)}}.

view(I) ->
	{{number, CarNumber}, _, _} = I,
	{CarNumber, {erTest_car, start_link, [I]}, permanent, 2000, worker, [erTest_car]}.

% print_info(Cars) ->
% 	io:format("Loaded data about cars~n"),
% 	io:format("~p~n", [Cars]),
% 	io:format("You can use erTest_car:car_start(Number) to start car~n"),
% 	io:format("You can use erTest_car:car_stop(Number) to stop car~n"),
% 	io:format("You can use erTest_car:car_position(Number) to know current car position~n").

route(0) -> [];
route(Q) ->
	[erTest_car:km_to_deg({(random:uniform() - 0.5) * ?MAPSIZE, (random:uniform() - 0.5) * ?MAPSIZE})|route(Q - 1)].

generate_routes(0) -> [];
generate_routes(Q) -> 
	[route(random:uniform(7) + 3)|generate_routes(Q - 1)].

generate_cars(0, _Routes) -> [];
generate_cars(Number, Routes) ->
	I = {{number, Number}, {route, lists:nth(random:uniform(?ROUTES), Routes)}, {speed, random:uniform(30) + 60}},
	[I|generate_cars(Number - 1, Routes)].
