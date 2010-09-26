-module(rabbit_client).

%% API
-export([open_connection/0,
         close_connection/1,
         open_channel/1,
         close_channel/1,
         create_exchange/3,
         create_queue/2,
         bind_queue/4,
         subscribe_to_queue/2]).

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
    Cmd = #'exchange.declare'{exchange = Name, type = Type},
    #'exchange.declare_ok'{} = amqp_channel:call(Channel, Cmd),
    Name.

create_queue(Name, Channel) when is_binary(Name), is_pid(Channel) ->
    Cmd = #'queue.declare'{queue = Name},
    #'queue.declare_ok'{queue = Name} = amqp_channel:call(Channel, Cmd),
    Name.

bind_queue(Exchange, Queue, Binding, Channel) when is_binary(Exchange), is_binary(Queue), is_binary(Binding), is_pid(Channel) ->
    Cmd = #'queue.bind'{queue = Queue, exchange = Exchange, routing_key = Binding},
    #'queue.bind_ok'{} = amqp_channel:call(Channel, Cmd),
    ok.

subscribe_to_queue(Queue, Channel) when is_binary(Queue), is_pid(Channel) ->
    Cmd = #'basic.consume'{queue = Queue},
    #'basic.consume_ok'{consumer_tag = ConsumerTag} = amqp_channel:subscribe(Channel, Cmd, self()),
    ConsumerTag.

%%%===================================================================
%%% Internal functions
%%%===================================================================
