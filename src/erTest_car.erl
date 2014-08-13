-module(erTest_car).

-behaviour(gen_fsm).

-export([start_link/1]).

-export([init/1, moving/2, handle_event/3,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(status, {
		position,
		number,
		speed,
		route,
		current_route
	}).

start_link(I) ->
	gen_fsm:start_link(?MODULE, [I], []).

init([I]) ->
	{{number, CarNumber}, {route, CarRoute}, {speed, CarSpeed}} = I, 
	[CarPosition|_] = CarRoute,
	{ok, moving, #status{position = CarPosition, number = CarNumber, speed = CarSpeed, route = CarRoute, current_route = CarRoute}, 1000}.

moving(timeout, CarStatus) -> 
	#status{number = CarNumber} = CarStatus,
	{NewPosition, NewRoute} = nextpoint(CarStatus),
	io:format("Car ~p now at ~p~n", [CarNumber, NewPosition]),
	{next_state, moving, CarStatus#status{position = NewPosition, current_route = NewRoute}, 1000}.


handle_event(_Event, StateName, State) ->
  {next_state, StateName, State}.

handle_sync_event(_Event, _From, StateName, State) ->
  Reply = {error, invalid_message},
  {reply, Reply, StateName, State}.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


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