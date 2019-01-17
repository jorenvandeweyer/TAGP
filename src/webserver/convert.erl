-module(convert).
-export([pid_to_bin/1, pid_to_bin/2]).
-export([bin_to_pid/1]).

pid_to_bin(Pid) ->
    List = erlang:pid_to_list(Pid),
    Binary = erlang:list_to_binary(List),
    Binary.
pid_to_bin(pid, Pid) ->
    pid_to_bin(Pid);
pid_to_bin(list, List) ->
    lists:map(fun pid_to_bin/1, List).

bin_to_pid(Bin) ->
    List = erlang:binary_to_list(Bin),
    Pid = erlang:list_to_pid(List),
    Pid.
