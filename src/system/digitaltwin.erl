-module(digitaltwin).

-export([start_link/0, test/0]).
-export([init/0, init_resources/0, get_data/0, make_flowMeter/2, set_fluidum/1, make_heatExchanger/0,
        make_pipe/0, make_pump/0]).

start_link() ->
  Pid = spawn(?MODULE, init, []),
  register(?MODULE, Pid),
  {ok, Pid}.

init() ->
  survivor:start(),
  digitaltwin_server:start_link(),
  init_resources().

init_resources() ->
  {_, FlowMeterTyp_Pid} = resource_type:create(flowMeterTyp, []),
  {_, FluidumTyp_Pid} = resource_type:create(fluidumTyp, []),
  {_, HeatExchangerTyp_Pid} = resource_type:create(heatExchangerTyp, []),
  {_, PipeTyp_Pid} = resource_type:create(pipeTyp, []),
  {_, PumpTyp_Pid} = resource_type:create(pumpTyp, []),
  digitaltwin_server:set_ResourceType(flowMeterTyp, FlowMeterTyp_Pid),
  digitaltwin_server:set_ResourceType(fluidumTyp, FluidumTyp_Pid),
  digitaltwin_server:set_ResourceType(heatExchangerTyp, HeatExchangerTyp_Pid),
  digitaltwin_server:set_ResourceType(pipeTyp, PipeTyp_Pid),
  digitaltwin_server:set_ResourceType(pumpTyp, PumpTyp_Pid),
  digitaltwin_server:delete_AllResourceInstances().

get_data() ->
  ResourceInstances = lists:reverse(digitaltwin_server:get_ResourceInstances()),
  #{resource_data => lists:reverse(get_data(ResourceInstances))}.
get_data([]) -> [];
get_data([ResInst]) ->
  [#{num => 1, type => element(1, ResInst)}];
get_data(ResourceInstances) ->
  [FirstResInst | _] = ResourceInstances,
  get_data(ResourceInstances, FirstResInst, []).
get_data(ResourceInstances, ResInst, []) ->
  {ok, [_ConnIN, ConnOUT]} = resource_instance:list_connectors(element(2, ResInst)),
  Connected = connector:get_connected(ConnOUT),
  case Connected of
    {error, _, _, _, _} ->
      [#{num => 1, type => element(1, ResInst)}];
    {ok, disconnected} -> [#{num => 1, type => element(1, ResInst)}];
    _Else ->
      {ok, NextResInst_Pid} = connector:get_ResInst(element(2, Connected)),
      NextResInst = lists:keyfind(NextResInst_Pid, 2, ResourceInstances),
      if
        NextResInst =:= false -> [#{num => 1, type => element(1, ResInst)}];
        true -> get_data(ResourceInstances, NextResInst, [#{num => 1, type => element(1, ResInst)}])
      end
  end;
get_data(ResourceInstances, ResInst, Result) ->
  [Prev_Result | _] = Result,
  {ok, [_ConnIN, ConnOUT]} = resource_instance:list_connectors(element(2, ResInst)),
  Connected = connector:get_connected(ConnOUT),
  case Connected of
    {error, _, _, _, _} ->
      [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result];
    {ok, disconnected} -> [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result];
    _Else ->
      {ok, NextResInst_Pid} = connector:get_ResInst(element(2, Connected)),
      NextResInst = lists:keyfind(NextResInst_Pid, 2, ResourceInstances),
      if
        NextResInst =:= false -> [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result];
        true -> get_data(ResourceInstances, NextResInst, [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result])
      end
  end.

test() ->
  {ok, A} = make_pipe(), {ok, B} = make_pipe(), {ok, C} = make_pipe(), {ok, D} = make_pipe(),
  {ok, [IN_A, OUT_A]} = resource_instance:list_connectors(A),
  {ok, [IN_B, _OUT_B]} = resource_instance:list_connectors(B),
  connector:connect(OUT_A, IN_B),
  {ok, F_Pid} = set_fluidum(IN_A),
  {ok, [Loc_A]} = resource_instance:list_locations(A),
  location:arrival(Loc_A, F_Pid),
  {ok, FlowMeterInst_Pid} = make_flowMeter(B, fun(Arg)-> Arg*2 end),
  {pipeA, A, outA, OUT_A, pipeB, B, inB, IN_B, pipeC, C, pipeD, D, f_pid, F_Pid, loc_A, Loc_A, flowmeterpid, FlowMeterInst_Pid}.

make_flowMeter(ResInst_Pid, RealWorldCmdFn) ->
  FlowMeterTyp_Pid = digitaltwin_server:get_ResourceType(flowMeterTyp),
  {ok, FlowMeterInst_Pid} = resource_instance:create(flowMeterInst, [digitaltwin, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn]),
  digitaltwin_server:add_ResourceInstance(flowMeterInst, FlowMeterInst_Pid),
  {ok, FlowMeterInst_Pid}.

set_fluidum(Root_ConnectorPid) ->
  FluidumTyp_Pid = digitaltwin_server:get_ResourceType(fluidumTyp),
  {ok, FluidumInst_Pid} = resource_instance:create(fluidumInst, [Root_ConnectorPid, FluidumTyp_Pid]),
  digitaltwin_server:set_FluidumInstance(FluidumInst_Pid),
  {ok, FluidumInst_Pid}.

make_heatExchanger() -> ok.

make_pipe() ->
  PipeTyp_Pid = digitaltwin_server:get_ResourceType(pipeTyp),
  {ok, PipeInst_Pid} = resource_instance:create(pipeInst, [digitaltwin, PipeTyp_Pid]),
  digitaltwin_server:add_ResourceInstance(pipeInst, PipeInst_Pid),
  %PipeInst_Pids = digitaltwin_server:get_ResourceInstances(),
  %if
  %  length(PipeInst_Pids) > 1 ->
  %    [A, B | _] = PipeInst_Pids,
  %    {ok, [_IN1, OUT1]} = resource_instance:list_connectors(B),
  %    {ok, [IN2, _OUT2]} = resource_instance:list_connectors(A),
  %    connector:connect(OUT1, IN2);
  %  true -> ok
  %end,
  {ok, PipeInst_Pid}.

make_pump() -> ok.


