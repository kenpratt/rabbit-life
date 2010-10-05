-module(board).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-include("logging.hrl").

-define(SERVER, ?MODULE).
-define(TICK_INTERVAL, 10000). % in milliseconds

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
    {ok, #state{board = new_board(), tick_timer = TRef, connection = Connection, channel = Channel}}.

handle_call(Request, _From, State) ->
    ?log_info("Received unexpected call: ~p", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    ?log_info("Received unexpected cast: ~p", [Msg]),
    {noreply, State}.

handle_info(tick, State) ->
    ?log_info("Tick", []),
    {noreply, State};

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
    ?log_info("Incoming message: ~p, ~128p", [Topic, DecodedContent]),
    handle_message(Topic, DecodedContent, State).

handle_message(<<"life.board.add">>, Props, #state{board = Board} = State) ->
    Cells = proplists:get_value(cells, Props),
    ?log_info("Got cells: ~p", [Cells]),
    Board2 = set_cells(Cells, Board),
    ?log_info("Board: ~n~p", [Board2]),
    {noreply, State#state{board = Board2}}.

new_board() ->
    lists:foldl(fun(Y, A) ->
                        array:set(Y, array:new(100), A)
                end, array:new(100), lists:seq(0,99)).

get_cell(X, Y, Board) ->
    Row = array:get(Y, Board),
    array:get(X, Row).

set_cell(X, Y, Colour, Board) ->
    ?log_info("Setting ~p,~p to ~p", [X, Y, Colour]),
    Row = array:get(Y, Board),
    Row2 = array:set(X, Colour, Row),
    array:set(Y, Row2, Board).

set_cells([], Board) ->
    Board;
set_cells([Cell|Rest], Board) ->
    set_cells(Rest, set_cell(proplists:get_value(x, Cell), proplists:get_value(y, Cell), proplists:get_value(c, Cell), Board)).
