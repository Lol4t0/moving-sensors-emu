-module(erTest_sup).

-behaviour(supervisor).

-export([start_link/4]).

-export([init/1]).

start_link(Routes, Cars, StartNumber, Name) ->
	supervisor:start_link({local, sup_to_atom(Name)}, ?MODULE, [Routes, Cars, StartNumber]).

init([Routes, CarsNumber, StartNumber]) ->
	Cars = lists:map(fun(N) -> generate_car(N, Routes, StartNumber) end, lists:seq(1, CarsNumber)),
	{ok,{{one_for_one,5,10}, Cars}}.

generate_car(Number, Routes, StartNumber) ->
	Route = lists:nth(random:uniform(erlang:length(Routes)), Routes),
	I = {{number, Number + StartNumber}, {route, Route}, {speed, random:uniform(30) + 60}},
	{Number, {erTest_car, start_link, [I]}, permanent, 2000, worker, [erTest_car]}.

sup_to_atom(Number) ->
	list_to_atom(lists:flatten(io_lib:format("erTest_sup~p", [Number]))).