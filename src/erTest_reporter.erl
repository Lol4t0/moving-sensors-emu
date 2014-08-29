-module(erTest_reporter).

-export([report_position/2]).

report_position(CarNumber, PositionData) ->
	{Lon, Lat} = PositionData,
	% io:format("~p ~p~n", [Lon, Lat]).
	websocket ! {position, io_lib:format("[~p,~p,~p]", [CarNumber, Lon, Lat])}.	
