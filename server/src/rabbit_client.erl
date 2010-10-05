-module(rabbit_client).

%% API
-export([open_connection/0,
         close_connection/1,
         open_channel/1,
         close_channel/1,
         create_exchange/3,
         create_queue/2,
         bind_queue/4,
         subscribe_to_queue/2,
         publish/4,
         is_amqp_message/1,
         get_topic/1,
         get_content/1]).

-include("logging.hrl").
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
    ?log_info("Channel closed", []),
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

publish(Exchange, Key, Message, Channel) when is_binary(Exchange), is_binary(Key), is_binary(Message), is_pid(Channel) ->
    Publish = #'basic.publish'{exchange = Exchange, routing_key = Key},
    ok = amqp_channel:call(Channel, Publish, #amqp_msg{payload = Message}).

is_amqp_message({#'basic.deliver'{}, #'amqp_msg'{}}) ->
    true;
is_amqp_message(_) ->
    false.

get_topic({#'basic.deliver'{} = Tag, #'amqp_msg'{}}) ->
    get_topic(Tag);
get_topic(#'basic.deliver'{routing_key = RoutingKey}) ->
    RoutingKey.

get_content({#'basic.deliver'{}, #'amqp_msg'{} = Msg}) ->
    get_content(Msg);
get_content(#'amqp_msg'{payload = Payload}) ->
    Payload.

%%%===================================================================
%%% Internal functions
%%%===================================================================
