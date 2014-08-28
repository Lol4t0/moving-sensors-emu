-module(erTest_msup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).
-define(ROUTES, 10).
-define(CARS, 5).
-define(SUPERVISORS, 2).
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
	Routes = generate_routes(?ROUTES),
	Supers = create_supervisors(1, Routes),
	{ok,{{one_for_one,5,10}, Supers}}.

create_supervisors(?SUPERVISORS, Routes) -> 
	Q = ?CARS div ?SUPERVISORS,
	StartNumber = (?SUPERVISORS - 1) * Q,
	Super = {?SUPERVISORS, {erTest_sup, start_link, [Routes, Q + ?CARS rem ?SUPERVISORS, StartNumber, ?SUPERVISORS]}, permanent, 2000, supervisor, [erTest_sup]},
	[Super];	
create_supervisors(Number, Routes) ->
	Q = ?CARS div ?SUPERVISORS,
	StartNumber = (Number - 1) * Q,
	Super = {Number, {erTest_sup, start_link, [Routes, Q, StartNumber, Number]}, permanent, 2000, supervisor, [erTest_sup]},
	[Super | create_supervisors(Number + 1, Routes)].

route(0) -> [];
route(Q) ->
	[erTest_car:km_to_deg({(random:uniform() - 0.5) * ?MAPSIZE, (random:uniform() - 0.5) * ?MAPSIZE})|route(Q - 1)].

generate_routes(0) -> [];
generate_routes(Q) -> 
	[route(random:uniform(7) + 3)|generate_routes(Q - 1)].