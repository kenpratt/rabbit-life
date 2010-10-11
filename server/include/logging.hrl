-define(log(Level, Pid, Module, Line, Fmt, Args),
        io:format("[~s] [~p] ~s:~p " ++ Fmt ++ "~n", [Level, Pid, Module, Line|Args])).
-define(log_debug(Fmt, Args),
        noop).
%        ?log(debug, self(), ?MODULE, ?LINE, Fmt, Args)).
-define(log_info(Fmt, Args),
        ?log(info, self(), ?MODULE, ?LINE, Fmt, Args)).
-define(log_warn(Fmt, Args),
        ?log(warn, self(), ?MODULE, ?LINE, Fmt, Args)).
-define(log_error(Fmt, Args),
        ?log(error, self(), ?MODULE, ?LINE, Fmt, Args)).
