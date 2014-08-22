-module(erTest_msup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(ROUTES, 10000).
-define(CARS, 100000).
-define(SUPERVISORS, 100).
-define(MAPSIZE, 100).

start_link() ->
	statistics(runtime),
	statistics(wall_clock),
	Super = supervisor:start_link({local, ?SERVER}, ?MODULE, []),
	{_, Time1} = statistics(runtime),
	{_, Time2} = statistics(wall_clock),
	io:format("Data loaded for = ~p (~p) seconds~n", [Time1 div 1000, Time2 div 1000]),
	Super.

init([]) ->
	Routes = lists:map(fun generate_route/1, lists:seq(1, ?ROUTES)),
	Supers = lists:map(fun(N) -> create_supervisor(N, Routes) end, lists:seq(1, ?SUPERVISORS)),
	{ok,{{one_for_one,5,10}, Supers}}.

create_supervisor(?SUPERVISORS, Routes) -> 
	Q = ?CARS div ?SUPERVISORS,
	StartNumber = (?SUPERVISORS - 1) * Q,
	{?SUPERVISORS, {erTest_sup, start_link, [Routes, Q + ?CARS rem ?SUPERVISORS, StartNumber, ?SUPERVISORS]}, permanent, 2000, supervisor, [erTest_sup]};	
create_supervisor(Number, Routes) ->
	Q = ?CARS div ?SUPERVISORS,
	StartNumber = (Number - 1) * Q,
	{Number, {erTest_sup, start_link, [Routes, Q, StartNumber, Number]}, permanent, 2000, supervisor, [erTest_sup]}.

route(_Number) ->
	erTest_car:km_to_deg({(random:uniform() - 0.5) * ?MAPSIZE, (random:uniform() - 0.5) * ?MAPSIZE}).

generate_route(_Number) -> 
	lists:map(fun route/1, lists:seq(1, random:uniform(7) + 3)).