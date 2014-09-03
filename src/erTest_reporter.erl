-module(erTest_reporter).

-export([report_position/2]).

report_position(CarNumber, PositionData) ->
	{Lon, Lat} = PositionData,
	websocket ! {position, io_lib:format("[~p,~p,~p]", [CarNumber, Lon, Lat])}.	
