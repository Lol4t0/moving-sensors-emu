-module(erTest_car).

-behaviour(gen_fsm).

-define(FREQ, 1000).
-define(MOSCOW_LON, 55.75).
-define(MOSCOW_LAT, 37.62).
-define(ONE_LAT, 111.3).
-define(ONE_LON, 111).

-export([start_link/1, car_stop/1, car_start/1, car_position/1, car_route/1]).

-export([init/1, moving/2, handle_event/3, stationary/2,
         handle_sync_event/4, handle_info/3, terminate/3, code_change/4,
         km_to_deg/1, deg_to_km/1]).

-record(status, {
		position,
		number,
		speed,
		route,
		current_route,
		res,
		time = milisecs()
	}).

start_link(I) ->
	gen_fsm:start_link(?MODULE, [I], []).

init([I]) ->
	{{number, CarNumber}, {route, CarRoute}, {speed, CarSpeed}} = I, 
	[CarPosition|_] = CarRoute,
	gproc:add_local_name(CarNumber),
	{ok, moving, #status{	position = CarPosition, 
							number = CarNumber, 
							speed = CarSpeed / 3600, 
							route = CarRoute, 
							current_route = CarRoute, 
							res = CarSpeed / 3600, 
							time = milisecs()}, ?FREQ}.

car_stop(CarNumber) ->
	gen_fsm:send_event(gproc:lookup_local_name(CarNumber), stop).
car_start(CarNumber) ->
	gen_fsm:send_event(gproc:lookup_local_name(CarNumber), start).
car_position(CarNumber) ->
	gen_fsm:sync_send_all_state_event(gproc:lookup_local_name(CarNumber), position).
car_route(CarNumber) ->
	gen_fsm:sync_send_all_state_event(gproc:lookup_local_name(CarNumber), route).

stationary(start, CarStatus) ->
	{next_state, moving, CarStatus, ?FREQ};
stationary(_Event, CarStatus) ->
	{next_state, stationary, CarStatus}.

moving(timeout, CarStatus) -> 
	{next_state, moving, nextpoint(CarStatus), ?FREQ};
moving(stop, CarStatus) ->
	{next_state, stationary, CarStatus};
moving(_Event, CarStatus = #status{time = T}) ->
	{next_state, moving, CarStatus, ?FREQ - milisecs() + T}.



handle_event(_Event, StateName, CarStatus) ->
	{next_state, StateName, CarStatus}.

handle_sync_event(position, _From, StateName, CarStatus = #status{position = CarPosition, time = T}) ->
	if 
		StateName == moving ->
			{reply, CarPosition, moving, CarStatus, ?FREQ - milisecs() + T};
		StateName == stationary ->
			{reply, CarPosition, stationary, CarStatus}
	end;
handle_sync_event(route, _From, StateName, CarStatus = #status{route = CarRoute, time = T}) ->
	if 
		StateName == moving ->
			{reply, CarRoute, moving, CarStatus, ?FREQ - milisecs() + T};
		StateName == stationary ->
			{reply, CarRoute, stationary, CarStatus}
	end.

handle_info(_Info, StateName, State) ->
  {next_state, StateName, State}.

terminate(_Reason, _StateName, _State) ->
  ok.

code_change(_OldVsn, StateName, State, _Extra) ->
  {ok, StateName, State}.


distance(Point1, Point2) ->
	{X1, Y1} = deg_to_km(Point1),
	{X2, Y2} = deg_to_km(Point2),
	math:sqrt((X2-X1)*(X2-X1)+(Y2-Y1)*(Y2-Y1)).

newposition(Point1, Point2, L) ->
	{X1, Y1} = deg_to_km(Point1),
	{X2, Y2} = deg_to_km(Point2),	
	km_to_deg({X1 + L*(X2-X1)/distance(Point1, Point2), Y1 + L*(Y2-Y1)/distance(Point1, Point2)}).

nextpoint(#status{current_route = [], route = OldRoute} = CarStatus) ->
	NewRoute = lists:reverse(OldRoute),
	nextpoint(CarStatus#status{route = NewRoute, current_route = NewRoute});
nextpoint(#status{	position = CarPosition, 
					current_route = CurrentRoute, 
					speed = CarSpeed,
					res = Residual} = CarStatus) ->
	[NextPoint|NewRoute] = CurrentRoute,
	L = distance(CarPosition, NextPoint),
	if
		L =< Residual ->
			nextpoint(CarStatus#status{position = NextPoint, current_route = NewRoute, res = Residual - L});
		L > Residual ->
			NewPosition = newposition(CarPosition, NextPoint, CarSpeed),
			erTest_reporter:report_position(CarStatus#status{	position = NewPosition, 
																current_route = CurrentRoute, 
																res = CarSpeed, 
																time = milisecs()})
	end.

milisecs() ->
	{MegaSecs, Secs, MicroSecs} = erlang:now(),
	MegaSecs * 1000000000 + Secs*1000 + (MicroSecs div 1000).

deg_to_km({Lon, Lat}) ->
	{Lon * ?ONE_LON, Lat * (?ONE_LAT * math:cos(Lon * math:pi() / 180))}.
km_to_deg({X, Y}) ->
	{X / ?ONE_LON, Y / (?ONE_LAT * math:cos((X / ?ONE_LON) * math:pi() / 180))}.
