-module(erTest_car).

-behaviour(gen_fsm).

-define(FREQ, 1000).

-export([start_link/1, stop_car/1, start_car/1, car_position/1]).

-export([init/1, moving/2, handle_event/3, stationary/2,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4]).

-record(status, {
		position,
		number,
		speed,
		route,
		current_route
	}).

start_link(I) ->
	{{number, CarNumber}, _, _} = I,
	gen_fsm:start_link({local, car_to_atom(CarNumber)}, ?MODULE, [I], []).

init([I]) ->
	{{number, CarNumber}, {route, CarRoute}, {speed, CarSpeed}} = I, 
	[CarPosition|_] = CarRoute,
	{ok, stationary, #status{position = CarPosition, number = CarNumber, speed = CarSpeed, route = CarRoute, current_route = CarRoute}}.

stop_car(CarNumber) ->
	gen_fsm:send_event(car_to_atom(CarNumber), stop).
start_car(CarNumber) ->
	gen_fsm:send_event(car_to_atom(CarNumber), start).
car_position(CarNumber) ->
	gen_fsm:sync_send_all_state_event(car_to_atom(CarNumber), position).

stationary(start, CarStatus) ->
	{next_state, moving, CarStatus, ?FREQ};
stationary(_Event, CarStatus) ->
	{next_state, stationary, CarStatus}.

moving(timeout, CarStatus = #status{number = CarNumber}) -> 
	{NewPosition, NewRoute} = nextpoint(CarStatus),
	erTest_reporter:report_position(CarNumber, NewPosition),
	{next_state, moving, CarStatus#status{position = NewPosition, current_route = NewRoute}, ?FREQ};
moving(stop, CarStatus) ->
	{next_state, stationary, CarStatus};
moving(_Event, CarStatus) ->
	{next_state, moving, CarStatus, ?FREQ}.



handle_event(_Event, StateName, CarStatus) ->
	{next_state, StateName, CarStatus}.

handle_sync_event(position, _From, StateName, CarStatus = #status{position = CarPosition}) ->
	if 
		StateName == moving ->
			{reply, CarPosition, moving, CarStatus, ?FREQ};
		StateName == stationary ->
			{reply, CarPosition, stationary, CarStatus}
	end.

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

car_to_atom(CarNumber) ->
	list_to_atom(lists:flatten(io_lib:format("erTest_car~p", [CarNumber]))).