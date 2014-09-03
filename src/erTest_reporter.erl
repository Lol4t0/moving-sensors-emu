-module(erTest_reporter).

-export([report_position/1]).

-record(status, {
		position,
		number,
		speed,
		route,
		current_route,
		res,
		time
	}).

report_position(#status{position={Lon, Lat}, number = CarNumber, speed = CarSpeed} = CarStatus) ->
	case whereis(websocket) of
		undefined -> wait;
		Pid -> Pid ! {position, jiffy:encode( {[{is_track, false}, {id, CarNumber}, {lon, Lon}, {lat, Lat}, {speed, CarSpeed * 3600}]} )}
	end,
	CarStatus.


