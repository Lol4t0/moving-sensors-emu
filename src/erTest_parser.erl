-module(erTest_parser).
-include_lib("xmerl/include/xmerl.hrl").

-export([main/0]).

-define(PATH, "/Projects/interesting_project").
-define(MASK, "\\..*(cpp|c)").

main() ->
    {#xmlElement{content=KML},_Miscg}=xmerl_scan:file(filename:join(code:priv_dir(erTest), "tracks.kml")),
    [#xmlElement{content=Doc}|_] = [A || A <- KML, is('Document', A)],
    Tracks = check_folders([A || A <- Doc, is('Folder', A)]),
    unconsult("tracks.dat", Tracks),
    ok.

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

check_folders([]) -> [];
check_folders([Folder|Tail]) ->
    #xmlElement{content=Folder_content} = Folder,
    Placemarks = [A || A <- Folder_content, is('Placemark', A)],
    Tracks = lists:map(fun get_track/1, Placemarks),
    lists:merge(lists:filter(fun void_filter/1, Tracks), check_folders(Tail)).

void_filter([]) -> false;
void_filter(_I) -> true.

get_track(Placemark) ->
    #xmlElement{content=Placemark_content} = Placemark,
    Tracks = [A || A <- Placemark_content, is('gx:Track', A)],
    case Tracks of
        [] -> [];
        [#xmlElement{content=Track}|_] ->            
            Coordinates = [A || A <- Track, is('gx:coord', A)],
            lists:map(fun get_point/1, Coordinates)
    end.

get_point(Coordinate) ->
    #xmlElement{content=Coordinate_content} = Coordinate,
    [#xmlText{value=Str}|_] = Coordinate_content,
    Points = my_split(lists:reverse(Str), [], []),
    {lists:nth(2, Points), lists:nth(1, Points)}.

unconsult(File, L) ->
    {ok, S} = file:open(File, write),
    lists:foreach(fun(X) -> io:format(S, "~p.~n" ,[X]) end, L),
    file:close(S).