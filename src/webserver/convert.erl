-module(convert).
-export([pid_to_bin/1, pid_to_bin/2]).

pid_to_bin(Pid) ->
    List = erlang:pid_to_list(Pid),
    Binary = list_to_binary(List),
    Binary.
pid_to_bin(pid, Pid) ->
    pid_to_bin(Pid);
pid_to_bin(list, List) ->
    lists:map(fun pid_to_bin/1, List).
