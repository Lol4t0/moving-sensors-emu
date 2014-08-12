-module(erTest_car).
-export([start/1]).

start(I) ->
	{_,{route,CarRoute},_} = I, 
	[CarPosition|_] = CarRoute,
	{ok, spawn (fun() -> car(I, CarPosition, CarRoute) end)}.

distance(Point1, Point2) ->
	{X1, Y1} = Point1,
	{X2, Y2} = Point2,
	math:sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)).

newposition(Point1, Point2, L) ->
	{X1, Y1} = Point1,
	{X2, Y2} = Point2,
	{X1 + L*(X2-X1)/distance(Point1, Point2), Y1 + L*(Y2-Y1)/distance(Point1, Point2)}.

nextpoint({CarPosition, [], FullRoute, CarSpeed}) ->
	nextpoint({CarPosition, FullRoute, FullRoute, CarSpeed});
nextpoint(I) ->
	{CarPosition, CurrentRoute, FullRoute, CarSpeed} = I,
	[NextPoint|NewRoute] = CurrentRoute,
	L = distance(CarPosition, NextPoint),
	if
		L =< CarSpeed ->
			nextpoint({NextPoint, NewRoute, FullRoute, CarSpeed - L});
		L > CarSpeed ->
			{newposition(CarPosition, NextPoint, CarSpeed), CurrentRoute}
	end.

car(I, CarPosition, CurrentRoute) ->
	receive
		after 1000 ->
			{{number,CarNumber},{route,FullRoute},{speed, CarSpeed}} = I,
			Info = {CarPosition, CurrentRoute, FullRoute, CarSpeed},
			{NewPosition, NewRoute} = nextpoint(Info),
			io:format("Car ~p now at ~p~n", [CarNumber, CarPosition]),
			car(I, NewPosition, NewRoute) 
	end.

