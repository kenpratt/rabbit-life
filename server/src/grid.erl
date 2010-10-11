-module(grid).

-export([new/2,
         get/2,
         set/3,
         active_cells/1]).

-record(grid, {width, height, data}).

%%%===================================================================
%%% API
%%%===================================================================

new(Width, Height) when is_integer(Width), is_integer(Height) ->
    Data = array:new(Width * Height),
    #grid{width = Width, height = Height, data = Data}.

get(Pos, #grid{data = Data} = Grid) ->
    case in_bounds(Pos, Grid) of
        true ->
            array:get(index_of(Pos, Grid), Data);
        false ->
            undefined
    end.

set(Pos, Value, #grid{data = Data} = Grid) ->
    case in_bounds(Pos, Grid) of
        true ->
            Grid#grid{data = array:set(index_of(Pos, Grid), Value, Data)};
        false ->
            Grid
    end.

active_cells(#grid{data = Data} = Grid) ->
    [{cell, index_to_position(I, Grid), Value} || {I, Value} <- array:sparse_to_orddict(Data)].

%%%===================================================================
%%% Internal functions
%%%===================================================================

in_bounds({X, Y}, #grid{width = Width, height = Height}) ->
    X >= 0 andalso X < Width andalso Y >= 0 andalso Y < Height.

index_of({X, Y}, #grid{width = Width}) ->
    (Y * Width) + X.

index_to_position(I, #grid{width = Width}) ->
    {I rem Width, I div Width}.
