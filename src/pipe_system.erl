-module(pipe_system).

-export([start/0]).

start() ->
    survivor:start(),
    observer:start(),

%% Create resource type: pipeType
    {ok, PipeType_PID} = pipeTyp:create(), % PID of pipeType !

    %% Create resource instances
    {ok, PipeInstance_0_PID} = pipeInst:create(self(), PipeType_PID),
    {ok, PipeInstance_1_PID} = pipeInst:create(self(), PipeType_PID),
    {ok, PipeInstance_2_PID} = pipeInst:create(self(), PipeType_PID),
    {ok, PipeInstance_3_PID} = pipeInst:create(self(), PipeType_PID),

    %% Create resource instance connectors
    {ok, [Connector_PipeInstance_0_PID_In, Connector_0_PipeInstance_PID_Out]} = resource_instance:list_connectors(PipeInstance_0_PID),
    {ok, [Connector_PipeInstance_1_PID_In, Connector_1_PipeInstance_PID_Out]} = resource_instance:list_connectors(PipeInstance_1_PID),
    {ok, [Connector_PipeInstance_2_PID_In, Connector_2_PipeInstance_PID_Out]} = resource_instance:list_connectors(PipeInstance_2_PID),
    {ok, [Connector_PipeInstance_3_PID_In, Connector_3_PipeInstance_PID_Out]} = resource_instance:list_connectors(PipeInstance_3_PID),

    %% Connect the connectors
    connector:connect(Connector_0_PipeInstance_PID_Out, Connector_PipeInstance_1_PID_In),
    connector:connect(Connector_1_PipeInstance_PID_Out, Connector_PipeInstance_2_PID_In),
    connector:connect(Connector_2_PipeInstance_PID_Out, Connector_PipeInstance_3_PID_In),
    connector:connect(Connector_3_PipeInstance_PID_Out, Connector_PipeInstance_0_PID_In),

    %% Get the locations
    {ok, [PipeInstance_0_location_PID]} = resource_instance:list_locations(PipeInstance_0_PID),
    {ok, [PipeInstance_1_location_PID]} = resource_instance:list_locations(PipeInstance_1_PID),
    {ok, [PipeInstance_2_location_PID]} = resource_instance:list_locations(PipeInstance_2_PID),
    {ok, [PipeInstance_3_location_PID]} = resource_instance:list_locations(PipeInstance_3_PID),

    {ok, {PipeInstance_0_location_type}} = location:get_Type(PipeInstance_0_location_PID),
    {ok, {PipeInstance_1_location_type}} = location:get_Type(PipeInstance_1_location_PID),
    {ok, {PipeInstance_2_location_type}} = location:get_Type(PipeInstance_2_location_PID),
    {ok, {PipeInstance_3_location_type}} = location:get_Type(PipeInstance_3_location_PID),

    {ok, PipeInstance_0_location_visitor_PID} = location:get_Visitor(PipeInstance_0_location_PID),
    {ok, PipeInstance_1_location_visitor_PID} = location:get_Visitor(PipeInstance_1_location_PID),
    {ok, PipeInstance_2_location_visitor_PID} = location:get_Visitor(PipeInstance_2_location_PID),
    {ok, PipeInstance_3_location_visitor_PID} = location:get_Visitor(PipeInstance_3_location_PID),

    io:fwrite("Started pipe_system~n", []),
    ok.
