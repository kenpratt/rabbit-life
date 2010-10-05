-module(board).

%% API
-export([new/0,
         get_cell/3,
         set_cell/4,
         set_cells/2,
         tick/1,
         to_proplist/1]).

-include("logging.hrl").

-define(WIDTH, 100).
-define(HEIGHT, 100).

-define(MAX_X, ?WIDTH - 1).
-define(MAX_Y, ?HEIGHT - 1).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    lists:foldl(fun(Y, A) ->
                        array:set(Y, array:new(?WIDTH), A)
                end, array:new(?HEIGHT), lists:seq(0,?MAX_Y)).

get_cell(X, Y, Board) ->
    case in_board(X, Y) of
        true ->
            Row = array:get(Y, Board),
            array:get(X, Row);
        false ->
            undefined
    end.

set_cell(X, Y, Colour, Board) ->
    Row = array:get(Y, Board),
    Row2 = array:set(X, Colour, Row),
    array:set(Y, Row2, Board).

set_cells([], Board) ->
    Board;
set_cells([Cell|Rest], Board) ->
    set_cells(Rest, set_cell(proplists:get_value(x, Cell), proplists:get_value(y, Cell), proplists:get_value(c, Cell), Board)).

tick(Board) ->
    tick_cell(0, 0, Board, Board).

to_proplist(Board) ->
    Cells = lists:append([[[{x,X},{y,Y},{c,C}] || {X, C} <- array:sparse_to_orddict(Row)] || {Y, Row} <- array:sparse_to_orddict(Board)]),
    [{board, [{cells, Cells}]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

in_board(X, Y) ->
    X >= 0 andalso X =< ?MAX_X andalso Y >= 0 andalso Y =< ?MAX_Y.

tick_cell(0, 100, _OldBoard, NewBoard) ->
    NewBoard;
tick_cell(100, Y, OldBoard, NewBoard) ->
    tick_cell(0, Y+1, OldBoard, NewBoard);
tick_cell(X, Y, OldBoard, NewBoard) ->
    Neighbours = live_neighbours(X, Y, OldBoard),
    Cell = get_cell(X, Y, OldBoard),
    Cell2 = next_state(Cell, Neighbours),
    NewBoard2 = set_cell(X, Y, Cell2, NewBoard),
    tick_cell(X+1, Y, OldBoard, NewBoard2).

live_neighbours(X, Y, Board) ->
    Positions = [{X-1,Y-1}, {X,Y-1}, {X+1,Y-1}, {X-1,Y}, {X+1,Y}, {X-1,Y+1}, {X,Y+1}, {X+1,Y+1}],
    Neighbours = [get_cell(NX, NY, Board) || {NX, NY} <- Positions],
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
