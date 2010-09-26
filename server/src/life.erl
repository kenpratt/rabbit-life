-module(life).

-export([start/0, stop/0]).

%% @spec start() -> ok
%% @doc Start the life server.
start() ->
    application:start(life).

%% @spec stop() -> ok
%% @doc Stop the life server.
stop() ->
    application:stop(life).
