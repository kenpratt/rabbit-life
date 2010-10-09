-module(player_registry).

%% API
-export([new/0,
         add_player/4,
         cull/1,
         to_proplist/1]).

-include("logging.hrl").

-record(player, {uuid, nick, colour}).

%%%===================================================================
%%% API
%%%===================================================================

new() ->
    dict:new().

add_player(Uuid, Nick, Colour, Registry) ->
    dict:store(Uuid, #player{uuid = Uuid, nick = Nick, colour = Colour}, Registry).

cull(Registry) ->
    Registry.

to_proplist(Registry) ->
    [{players, [player_to_proplist(P) || {_Uuid,P} <- dict:to_list(Registry)]}].

%%%===================================================================
%%% Internal functions
%%%===================================================================

player_to_proplist(#player{uuid = Uuid, nick = Nick, colour = Colour}) ->
    [{uuid, Uuid}, {nick, Nick}, {colour, Colour}].
