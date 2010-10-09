-module(player_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("logging.hrl").

-define(SERVER, ?MODULE).
-define(CULL_INTERVAL, 5000). % in milliseconds

-record(state, {players, cull_timer, connection, channel}).

%%%===================================================================
%%% API
%%%===================================================================

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

init([]) ->
    Connection = rabbit_client:open_connection(),
    Channel = rabbit_client:open_channel(Connection),

    %% set up RabbitMQ (operations are idempotent)
    rabbit_client:create_exchange(<<"life">>, <<"topic">>, Channel),
    rabbit_client:create_queue(<<"player_manager">>, Channel),
    rabbit_client:bind_queue(<<"life">>, <<"player_manager">>, <<"life.register">>, Channel),

    %% deliver new AMQP messages to our Erlang inbox
    rabbit_client:subscribe_to_queue(<<"player_manager">>, Channel),

    ?log_info("Player manager started", []),
    {ok, TRef} = timer:send_interval(?CULL_INTERVAL, cull),
    {ok, #state{players = player_registry:new(), cull_timer = TRef, connection = Connection, channel = Channel}}.

handle_call(Request, _From, State) ->
    ?log_info("Received unexpected call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?log_info("Received unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(cull, #state{players = Players} = State) ->
    ?log_info("Cull", []),
    case player_registry:cull(Players) of
        Players ->
            %% no change
            {noreply, State};
        Players2 ->
            State2 = State#state{players = Players2},
            broadcast_updated_players(State2),
            {noreply, State2}
    end;

handle_info(Info, State) ->
    case rabbit_client:is_amqp_message(Info) of
        true ->
            handle_raw_amqp_message(Info, State);
        false ->
            ?log_info("Received unexpected info: ~p", [Info]),
            {noreply, State}
    end.

terminate(Reason, #state{cull_timer = TRef, connection = Connection, channel = Channel}) ->
    ?log_info("Shutting down (reason: ~p)", [Reason]),
    timer:cancel(TRef),
    rabbit_client:close_channel(Channel),
    rabbit_client:close_connection(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

handle_raw_amqp_message(Message, State) ->
    Topic = rabbit_client:get_topic(Message),
    RawContent = rabbit_client:get_content(Message),
    DecodedContent = json:decode(RawContent),
    ?log_info("Incoming message: ~p, ~128p", [Topic, DecodedContent]),
    handle_message(Topic, DecodedContent, State).

handle_message(<<"life.register">>, Props, #state{players = Players} = State) ->
    Uuid = proplists:get_value(uuid, Props),
    Nick = proplists:get_value(nick, Props),
    Colour = proplists:get_value(colour, Props),
    ?log_info("New player: ~s, ~s (~s)", [Nick, Colour, Uuid]),
    Players2 = player_registry:add_player(Uuid, Nick, Colour, Players),
    State2 = State#state{players = Players2},
    send_to_player(Uuid, [{registered, true}], State),
    broadcast_updated_players(State2),
    {noreply, State2}.

send_to_player(Uuid, Message, State) ->
    publish(list_to_binary([<<"life.player.">>, Uuid]), Message, State).

broadcast_updated_players(#state{players = Players} = State) ->
    publish(<<"life.players.update">>, player_registry:to_proplist(Players), State).

publish(Topic, Message, #state{channel = Channel}) ->
    rabbit_client:publish(<<"life">>, Topic, json:encode(Message), Channel).
