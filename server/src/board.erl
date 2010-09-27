-module(board).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {connection, channel}).

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

    io:format("Board started~n", []),
    {ok, #state{connection = Connection, channel = Channel}}.

handle_call(Request, _From, State) ->
    io:format("Received unexpected call: ~p~n", [Request]),
    {reply, ok, State}.

handle_cast(Msg, State) ->
    io:format("Received unexpected cast: ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    case rabbit_client:is_amqp_message(Info) of
        true ->
            handle_raw_amqp_message(Info, State);
        false ->
            io:format("Received unexpected info: ~p~n", [Info]),
            {noreply, State}
    end.

terminate(Reason, #state{connection = Connection, channel = Channel}) ->
    io:format("Shutting down (reason: ~p)~n", [Reason]),
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
    io:format("~p, ~p~n", [Topic, DecodedContent]),
    handle_message(Topic, DecodedContent, State).

handle_message(<<"life.board.add">>, Props, State) ->
    Cells = proplists:get_value(cells, Props),
    io:format("Got cells: ~p~n", [Cells]),
    {noreply, State}.
