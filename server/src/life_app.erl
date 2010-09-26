-module(life_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    case life_sup:start_link() of
        {ok, Pid} ->
            io:format("Life server started~n", []),
            {ok, Pid};
        Other ->
            io:format("Life server failed to start: ~p~n", [Other]),
            {error, Other}
    end.

stop(_State) ->
    ok.
