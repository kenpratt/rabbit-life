-module(json).

%% API
-export([encode/1, decode/1]).

%%%===================================================================
%%% API
%%%===================================================================

encode(Term) ->
    list_to_binary(mochijson2:encode(pack(Term))).

decode(Raw) when is_binary(Raw) ->
    unpack(mochijson2:decode(Raw)).

%%%===================================================================
%%% Internal functions
%%%===================================================================

pack([{_,_}|_] = Proplist) ->
    {struct, [{Key, pack(Val)} || {Key, Val} <- Proplist]};
pack(List) when is_list(List) ->
    [pack(Val) || Val <- List];
pack(Term) ->
    Term.

unpack({struct, Proplist}) ->
    [{binary_to_atom(Key), unpack(Val)} || {Key, Val} <- Proplist];
unpack(List) when is_list(List) ->
    [unpack(Val) || Val <- List];
unpack(Term) ->
    Term.

binary_to_atom(Bin) when is_binary(Bin) ->
    list_to_atom(binary_to_list(Bin)).
