-module(erTest_car).
-export([start/1]).
-record(status, {
		position,
		number,
		speed,
		route,
		current_route
	}).

start(I) ->
	{{number, CarNumber}, {route, CarRoute}, {speed, CarSpeed}} = I, 
	[CarPosition|_] = CarRoute,
	CarStatus = #status{position = CarPosition, number = CarNumber, speed = CarSpeed, route = CarRoute, current_route = CarRoute},
	{ok, spawn (fun() -> car(CarStatus) end)}.

distance(Point1, Point2) ->
	{X1, Y1} = Point1,
	{X2, Y2} = Point2,
	math:sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)).

newposition(Point1, Point2, L) ->
	{X1, Y1} = Point1,
	{X2, Y2} = Point2,
	{X1 + L*(X2-X1)/distance(Point1, Point2), Y1 + L*(Y2-Y1)/distance(Point1, Point2)}.

nextpoint(#status{current_route = []} = CarStatus) ->
	#status{route = NewRoute} = CarStatus,
	nextpoint(CarStatus#status{current_route = NewRoute});
nextpoint(#status{position = CarPosition, current_route = CurrentRoute, speed = CarSpeed} = CarStatus) ->
	[NextPoint|NewRoute] = CurrentRoute,
	L = distance(CarPosition, NextPoint),
	if
		L =< CarSpeed ->
			nextpoint(CarStatus#status{position = NextPoint, current_route = NewRoute, speed = CarSpeed - L});
		L > CarSpeed ->
			{newposition(CarPosition, NextPoint, CarSpeed), CurrentRoute}
	end.

car(CarStatus) ->
	receive
		after 1000 ->
			#status{number = CarNumber} = CarStatus,
			{NewPosition, NewRoute} = nextpoint(CarStatus),
			io:format("Car ~p now at ~p~n", [CarNumber, NewPosition]),
			car(CarStatus#status{position = NewPosition, current_route = NewRoute}) 
	end.