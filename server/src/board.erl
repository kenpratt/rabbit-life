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

    rabbit_client:create_exchange(<<"life">>, <<"topic">>, Channel),

    io:format("Board started~n", []),
    {ok, #state{connection = Connection, channel = Channel}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, #state{connection = Connection, channel = Channel}) ->
    rabbit_client:close_channel(Channel),
    rabbit_client:close_connection(Connection),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
