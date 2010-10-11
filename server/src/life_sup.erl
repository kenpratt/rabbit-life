-module(life_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

init([]) ->
    BoardManager = {board_manager, {board_manager, start_link, []},
                    permanent, 2000, worker, dynamic},
    PlayerManager = {player_manager, {player_manager, start_link, []},
                     permanent, 2000, worker, dynamic},

    Children = [BoardManager, PlayerManager],
    RestartStrategy = {one_for_one, 4, 3600},
    {ok, {RestartStrategy, Children}}.
