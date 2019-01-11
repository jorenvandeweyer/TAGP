-module(pipe_system).
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2]).
-export([create_pipe/0, connect_pipes/2]).

start_link() ->
    survivor:start(),
    observer:start(),
    gen_server:start_link({local, pipe_system}, pipe_system, [], []).

init(_args) ->
    {ok, PipeType_PID} = pipeTyp:create(),
    {ok, #{pipes=>[], pipeType=>PipeType_PID}}.

handle_cast(reset, _Data) ->
    {noreply, #{pipes=>[]}};
handle_cast({add, Pipe}, Data0) ->
    Pipes0 = maps:get(pipes, Data0),
    Pipes = lists:reverse([Pipe|lists:reverse(Pipes0)]),
    Data = maps:update(pipes, Pipes, Data0),
    {noreply, Data}.

handle_call({get, Key}, _From, Data) ->
    Result = maps:get(Key, Data),
    {reply, Result, Data}.
% handle_call(get, _From, Data) ->
%     Pipes = maps:get(pipes, Data),
%     {reply, {Pipes}, Data}.

create_pipe() ->
    PipeType_PID = gen_server:call(pipe_system, {get, pipeType}),
    {ok, PipeInstance_PID} = pipeInst:create(self(), PipeType_PID),
    gen_server:cast(pipe_system, {add, PipeInstance_PID}),
    ok.

connect_pipes(Index1, Index2) ->
    Pipes = gen_server:call(pipe_system, {get, pipes}),
    Pipe1 = lists:nth(Index1, Pipes),
    Pipe2 = lists:nth(Index2, Pipes),
    {ok, [_In1, Out1]} = resource_instance:list_connectors(Pipe1),
    {ok, [In2, _Out2]} = resource_instance:list_connectors(Pipe2),
    connector:connect(Out1, In2),
    ok.
    % {ok, [PipeInstance_0_location_PID]} = resource_instance:list_locations(PipeInstance_0_PID),

    % {ok, {PipeInstance_0_location_type}} = location:get_Type(PipeInstance_0_location_PID),

    % {ok, PipeInstance_0_location_visitor_PID} = location:get_Visitor(PipeInstance_0_location_PID),
