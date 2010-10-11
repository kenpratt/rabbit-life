-module(player_registry).

%% API
-export([new/0,
         add_player/4,
         heartbeat/2,
         change_colour/3,
         cull/1,
         to_proplist/1]).

-include("logging.hrl").

-record(player, {uuid, nick, colour, last_active}).

-define(TIME_UNTIL_CULL, 8000).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    dict:new().

add_player(Uuid, Nick, Colour, Registry) ->
    dict:store(Uuid, #player{uuid = Uuid, nick = Nick, colour = Colour, last_active = now()}, Registry).

heartbeat(Uuid, Registry) ->
    case dict:is_key(Uuid, Registry) of
        true ->
            dict:update(Uuid, fun(P) -> P#player{last_active = now()} end, Registry);
        false ->
            Registry
    end.

change_colour(Uuid, Colour, Registry) ->
    case dict:is_key(Uuid, Registry) of
        true ->
            dict:update(Uuid, fun(P) -> P#player{colour = Colour, last_active = now()} end, Registry);
        false ->
            Registry
    end.

cull(Registry) ->
    Now = now(),
    dict:filter(fun(_Uuid, Player) -> is_alive(Player, Now) end, Registry).

to_proplist(Registry) ->
    [{players, [player_to_proplist(P) || P <- players(Registry)]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

is_alive(#player{last_active = Time}, Now) ->
    DiffInMillis = timer:now_diff(Now, Time) / 1000,
    DiffInMillis < ?TIME_UNTIL_CULL.

players(Registry) ->
    lists:sort(fun(#player{nick = N1}, #player{nick = N2}) -> N1 =< N2 end,
               [P || {_Uuid,P} <- dict:to_list(Registry)]).

player_to_proplist(#player{uuid = Uuid, nick = Nick, colour = Colour}) ->
    [{uuid, Uuid}, {nick, Nick}, {colour, Colour}].
