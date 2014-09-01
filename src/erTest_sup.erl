-module(erTest_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([start_all_cars/0, stop_all_cars/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(ROUTES, 1000).
-define(CARS, 100).
-define(MAPSIZE, 100).

start_link() ->
	supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
	Routes = lists:map(fun generate_route/1, lists:seq(1, ?ROUTES)),
	Cars = lists:map(fun(N) -> generate_car(N, Routes) end, lists:seq(1, ?CARS)),
	{ok,{{one_for_one,5,10}, Cars}}.

route(_Number) ->
	erTest_car:km_to_deg({(random:uniform() - 0.5) * ?MAPSIZE, (random:uniform() - 0.5) * ?MAPSIZE}).

generate_route(_Number) -> 
	lists:map(fun route/1, lists:seq(1, random:uniform(7) + 3)).

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