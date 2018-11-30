-module(pipe_system).

-export([start/0]).

start() ->
    survivor:start(),
    observer:start(),

    io:fwrite("Started pipe_system~n", []),
    ok.
