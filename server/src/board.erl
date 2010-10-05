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
            <<"#0000ff">>;
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
