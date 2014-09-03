-module(erTest_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([start_all_cars/0, stop_all_cars/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(CARS, 100).
-define(MOSCOW_LON, 55.75).
-define(MOSCOW_LAT, 37.62).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	Routes = read_routes(filename:join(code:priv_dir(erTest), "tracks.dat")),
	Cars = lists:map(fun(N) -> generate_car(N, Routes) end, lists:seq(1, ?CARS)),
	{ok,{{one_for_one, 10000, 1}, Cars}}.

read_routes(Path) ->
	{ok, Routes} = file:consult(Path),
	Routes.

generate_car(Number, Routes) ->
	Route = lists:nth(random:uniform(erlang:length(Routes)), Routes),
	I = {{number, Number}, {route, Route}, {speed, random:uniform(30) + 60}},
	{Number, {erTest_car, start_link, [I]}, permanent, 2000, worker, [erTest_car]}.

start_all_cars() -> 
	lists:map(fun erTest_car:car_start/1, lists:seq(1, ?CARS)),
	ok.

stop_all_cars() -> 
	lists:map(fun erTest_car:car_stop/1, lists:seq(1, ?CARS)),
	ok.