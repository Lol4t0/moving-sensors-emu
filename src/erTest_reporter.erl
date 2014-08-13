-module(erTest_reporter).

-export([report_position/2]).

report_position(CarNumber, PositionData) ->
	lager:info("Car ~p position: ~p", [CarNumber, PositionData]).