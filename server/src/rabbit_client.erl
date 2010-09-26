-module(rabbit_client).

%% API
-export([open_connection/0,
         close_connection/1,
         open_channel/1,
         close_channel/1,
         create_exchange/3]).

-include("amqp_client.hrl").

%%%===================================================================
%%% API
%%%===================================================================

open_connection() ->
    {ok, Connection} = amqp_connection:start(direct),
    Connection.

close_connection(Connection) when is_pid(Connection) ->
    ok = amqp_connection:close(Connection).

open_channel(Connection) when is_pid(Connection) ->
    {ok, Channel} = amqp_connection:open_channel(Connection),
    Channel.

close_channel(Channel) when is_pid(Channel) ->
    io:format("channel closed~n", []),
    ok = amqp_channel:close(Channel).

create_exchange(Name, Type, Channel) when is_binary(Name), is_binary(Type), is_pid(Channel) ->
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, #'exchange.declare'{exchange = Name, type = Type}),
    Name.

%%%===================================================================
%%% Internal functions
%%%===================================================================
