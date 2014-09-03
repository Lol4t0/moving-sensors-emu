-module(erTest_handler).
-behaviour(cowboy_websocket_handler).

-export([init/3]).
-export([websocket_init/3]).
-export([websocket_handle/3]).
-export([websocket_info/3]).
-export([websocket_terminate/3]).

init({tcp, http}, _Req, _Opts) ->
    {upgrade, protocol, cowboy_websocket}.

websocket_init(_TransportName, Req, _Opts) ->
	erlang:register(websocket, self()),
	% erTest_sup:start_all_cars(),
    {ok, Req, undefined_state}.

websocket_handle({text, Msg}, Req, State) ->
    Route = erTest_car:car_route(get_id(binary_to_list(Msg))),
    Reply = jiffy:encode( {[{is_track, true}, {track, lists:map(fun point_to_json/1, Route)} ]}),
    {reply, {text, Reply}, Req, State};
websocket_handle(_Data, Req, State) ->
    {ok, Req, State}.

websocket_info({position, Msg}, Req, State) ->
    {reply, {text, Msg}, Req, State};
websocket_info(_Info, Req, State) ->
    {ok, Req, State}.

websocket_terminate(_Reason, _Req, _State) ->
	erTest_sup:stop_all_cars(),
    ok.

get_id(Str) ->
	{Id, _Any} = string:to_integer(Str),
	Id.

point_to_json({Lon, Lat}) ->
	{[{lon, Lon}, {lat, Lat}]}.