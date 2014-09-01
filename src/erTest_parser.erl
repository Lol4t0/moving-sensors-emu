-module(erTest_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([main/0]).

-define(PATH, "/Projects/interesting_project").
-define(MASK, "\\..*(cpp|c)").

main() ->
    {#xmlElement{content=KML},_Miscg}=xmerl_scan:file("priv/MMO.kml"),
    [#xmlElement{content=Doc}|_] = [A || A <- KML, is('Document', A)],
    [#xmlElement{content=Track_folder}|_] = [A || A <- Doc, is('Folder', A)],
    [#xmlElement{content=Placemark}|_] = [A || A <- Track_folder, is('Placemark', A)],
    [#xmlElement{content=Tracks}|_] = [A || A <- Placemark, is('gx:Track', A)],
    [#xmlElement{content=Coordinates}|_] = [A || A <- Tracks, is('gx:coord', A)],
    [#xmlText{value=Str}|_] = Coordinates,
    my_split(lists:reverse(Str), [], []).

is(Type, A) ->
    if 
        is_record(A, xmlElement) ->
            #xmlElement{name=Name} = A,
            if 
                Name =:= Type -> true;
                true -> false
            end;
        true -> false
    end.

my_split([], Word, Array) -> [erlang:list_to_float(Word)|Array];
my_split(Str, Word, Array) ->
    [First|Tail] = Str,
    if 
        First =:= 32 ->
            my_split(Tail, [], [erlang:list_to_float(Word)|Array]);
        true -> my_split(Tail, [First|Word], Array)
    end.