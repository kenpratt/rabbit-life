-module(life_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("logging.hrl").

start(_StartType, _StartArgs) ->
    ?log_info("Starting life server", []),
    case life_sup:start_link() of
        {ok, Pid} ->
            ?log_info("Life server started", []),
            {ok, Pid};
        Other ->
            ?log_error("Life server failed to start: ~p", [Other]),
            {error, Other}
    end.

stop(_State) ->
    ok.
