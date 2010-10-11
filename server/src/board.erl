-module(board).

%% API
-export([new/0,
         set_cells/2,
         tick/1,
         to_proplist/1]).

-include("logging.hrl").

-define(WIDTH, 200).
-define(HEIGHT, 200).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    grid:new(?WIDTH, ?HEIGHT).

set_cells([], Board) ->
    Board;
set_cells([Cell|Rest], Board) ->
    X = proplists:get_value(x, Cell),
    Y = proplists:get_value(y, Cell),
    Colour = proplists:get_value(c, Cell),
    set_cells(Rest, grid:set({X, Y}, Colour, Board)).

tick(Board) ->
    Positions = positions_to_check(Board),
    do_tick(Positions, Board, new()).

to_proplist(Board) ->
    [{board, [{cells, [[{x, X}, {y, Y}, {c, Colour}] || {cell, {X, Y}, Colour} <- grid:active_cells(Board)]}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% check each active cell and all of its neighbours
positions_to_check(Board) ->
    Set = lists:foldl(fun({cell,Pos,_}, Set) -> sets:union(Set, sets:from_list([Pos|neighbour_positions(Pos)])) end, sets:new(), grid:active_cells(Board)),
    sets:to_list(Set).

neighbour_positions({X, Y}) ->
    [{X-1,Y-1}, {X,Y-1}, {X+1,Y-1}, {X-1,Y}, {X+1,Y}, {X-1,Y+1}, {X,Y+1}, {X+1,Y+1}].

do_tick([], _, NewBoard) ->
    NewBoard;
do_tick([Pos|Rest], OldBoard, NewBoard) ->
    Cell = grid:get(Pos, OldBoard),
    Neighbours = live_neighbours(Pos, OldBoard),
    case next_state(Cell, Neighbours) of
        undefined ->
            do_tick(Rest, OldBoard, NewBoard);
        Value ->
            do_tick(Rest, OldBoard, grid:set(Pos, Value, NewBoard))
    end.

live_neighbours(Pos, Board) ->
    Neighbours = [grid:get(N, Board) || N <- neighbour_positions(Pos)],
    lists:filter(fun(E) -> E =/= undefined end, Neighbours).

next_state(undefined, Neighbours) ->
    case length(Neighbours) =:= 3 of
        true ->
            %% a new cell is born
            blend_colours(Neighbours);
        false ->
            undefined
    end;

next_state(Cell, Neighbours) ->
    case length(Neighbours) of
        N when N =< 1 ->
            %% dies of loneliness
            undefined;
        N when N =:= 2 orelse N =:= 3 ->
            %% lives
            Cell;
        N when N >= 4 ->
            %% dies of overpopulation
            undefined
    end.

blend_colours(Cells) ->
    Colours = [hex_to_rgb(HexColour) || HexColour <- Cells],
    Aggregate = lists:foldl(fun({rgb, R, G, B}, {rgb, OutR, OutG, OutB}) ->
                                    {rgb, OutR + R, OutG + G, OutB + B}
                            end, {rgb, 0, 0, 0}, Colours),
    Num = length(Colours),
    {rgb, AggR, AggG, AggB} = Aggregate,
    rgb_to_hex({rgb, round(AggR/Num), round(AggG/Num), round(AggB/Num)}).

%% hex_to_rgb(<<"#FC3A80">>) -> {rgb, 252, 58, 128}
hex_to_rgb(Bin) ->
    Str = binary_to_list(Bin),
    R = hexstr_to_int(string:sub_string(Str, 2, 3)),
    G = hexstr_to_int(string:sub_string(Str, 4, 5)),
    B = hexstr_to_int(string:sub_string(Str, 6, 7)),
    {rgb, R, G, B}.

%% rgb_to_hex({rgb, 252, 58, 128}) -> <<"#FC3A80">>
rgb_to_hex({rgb, R, G, B}) ->
    list_to_binary(["#", int_to_hexstr(R), int_to_hexstr(G), int_to_hexstr(B)]).

hexstr_to_int(Str) ->
    httpd_util:hexlist_to_integer(Str).
int_to_hexstr(Int) ->
    Str = httpd_util:integer_to_hexlist(Int),
    case length(Str) of
        1 -> "0" ++ Str;
        _ -> Str
    end.
