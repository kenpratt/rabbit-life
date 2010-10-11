-module(board_manager).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("logging.hrl").

-define(SERVER, ?MODULE).
-define(TICK_INTERVAL, 500). % in milliseconds

-record(state, {board, tick_timer, connection, channel}).

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
    rabbit_client:create_queue(<<"board_changes">>, Channel),
    rabbit_client:bind_queue(<<"life">>, <<"board_changes">>, <<"life.board.add">>, Channel),

    %% deliver new AMQP messages to our Erlang inbox
    rabbit_client:subscribe_to_queue(<<"board_changes">>, Channel),

    ?log_info("Board started", []),
    {ok, TRef} = timer:send_interval(?TICK_INTERVAL, tick),
    {ok, #state{board = board:new(), tick_timer = TRef, connection = Connection, channel = Channel}}.

handle_call(Request, _From, State) ->
    ?log_info("Received unexpected call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?log_info("Received unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(tick, #state{board = Board} = State) ->
    ?log_debug("Tick", []),
    Board2 = board:tick(Board),
    State2 = State#state{board = Board2},
    broadcast_updated_board(State2),
    {noreply, State2};

handle_info(Info, State) ->
    case rabbit_client:is_amqp_message(Info) of
        true ->
            handle_raw_amqp_message(Info, State);
        false ->
            ?log_info("Received unexpected info: ~p", [Info]),
            {noreply, State}
    end.

terminate(Reason, #state{tick_timer = TRef, connection = Connection, channel = Channel}) ->
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
    ?log_debug("Incoming message: ~p, ~128p", [Topic, DecodedContent]),
    handle_message(Topic, DecodedContent, State).

handle_message(<<"life.board.add">>, Props, #state{board = Board} = State) ->
    Cells = proplists:get_value(cells, Props),
    ?log_info("New cells: ~p", [Cells]),
    Board2 = board:set_cells(Cells, Board),
    State2 = State#state{board = Board2},
    broadcast_updated_board(State2),
    {noreply, State2}.

broadcast_updated_board(#state{board = Board, channel = Channel}) ->
    rabbit_client:publish(<<"life">>, <<"life.board.update">>, json:encode(board:to_proplist(Board)), Channel).
