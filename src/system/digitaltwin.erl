-module(digitaltwin).

-export([start_link/0, test/0]).
-export([init/0, init_resources/0, get_data/0, add_resource/1]).

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
  get_data(ResourceInstances).
get_data([]) -> #{resource_data=>[]};
get_data([ResInst|_]) ->
  {ok, [_IN, OUT]} = resource_instance:list_connectors(element(2,ResInst)),
  fluidumTyp:discover_circuit(OUT).

connect_ResourceInstances([]) ->
  {ok, empty};
connect_ResourceInstances([FirstResInst | RemainingResInst]) ->
  connect_ResourceInstances(RemainingResInst, FirstResInst).

connect_ResourceInstances([LastResInst], PrevResInst) ->
  make_connection(PrevResInst, LastResInst),
  {ok, LastResInst};
connect_ResourceInstances([CurrentResInst | RemainingResInst], PrevResInst) ->
  make_connection(PrevResInst, CurrentResInst),
  connect_ResourceInstances(RemainingResInst, CurrentResInst).

make_connection(ResInst_A, ResInst_B) ->
  {ok, [_InA, OutA]} = resource_instance:list_connectors(ResInst_A),
  {ok, [InB, _OutB]} = resource_instance:list_connectors(ResInst_B),
  connector:connect(OutA, InB), connector:connect(InB, OutA).

make_flowMeter(ResInst_Pid, RealWorldCmdFn) ->
  FlowMeterTyp_Pid = digitaltwin_server:get_ResourceType(flowMeterTyp),
  {ok, FlowMeterInst_Pid} = resource_instance:create(flowMeterInst, [digitaltwin, FlowMeterTyp_Pid, ResInst_Pid, RealWorldCmdFn]),
  digitaltwin_server:add_ResourceInstance(flowMeterInst, FlowMeterInst_Pid),
  {ok, FlowMeterInst_Pid}.

make_heatExchanger() -> ok.

make_pipe(ResTyp_Pid) ->
  {ok, PipeInst_Pid} = resource_instance:create(pipeInst, [digitaltwin, ResTyp_Pid]),
  digitaltwin_server:add_ResourceInstance(pipeInst, PipeInst_Pid),
  {ok, PipeInst_Pid}.

make_pump() -> ok.

set_fluidum(Root_ConnectorPid) ->
  FluidumTyp_Pid = digitaltwin_server:get_ResourceType(fluidumTyp),
  {ok, FluidumInst_Pid} = resource_instance:create(fluidumInst, [Root_ConnectorPid, FluidumTyp_Pid]),
  digitaltwin_server:set_FluidumInstance(FluidumInst_Pid),
  {ok, FluidumInst_Pid}.

add_resource(flowMeterInst) ->
  FlowMeterTyp_Pid = digitaltwin_server:get_ResourceType(flowMeterTyp),
  {ok, PipeInst_Pid} = make_pipe(FlowMeterTyp_Pid),
  make_flowMeter(PipeInst_Pid, fun(Arg)-> Arg*2 end);
add_resource(heatExchangerInst) -> ok;
add_resource(pipeInst) -> test;
add_resource(pumpInst) -> ok.

test() ->
  PipeTyp_Pid = digitaltwin_server:get_ResourceType(pipeTyp),
  {ok, A} = make_pipe(PipeTyp_Pid), {ok, B} = make_pipe(PipeTyp_Pid), {ok, C} = make_pipe(PipeTyp_Pid), {ok, D} = make_pipe(PipeTyp_Pid),
  {ok, [IN_A, OUT_A]} = resource_instance:list_connectors(A),
  {ok, [IN_B, _OUT_B]} = resource_instance:list_connectors(B),
  {ok, F_Pid} = set_fluidum(IN_A),
  {ok, [Loc_A]} = resource_instance:list_locations(A),
  location:arrival(Loc_A, F_Pid),
  {ok, E} = make_pipe(PipeTyp_Pid),
  {ok, FlowMeterInst_Pid} = make_flowMeter(E, fun(Arg)-> Arg*2 end),
  connect_ResourceInstances([A, B, C, D, FlowMeterInst_Pid, A]),
  {pipeA, A, outA, OUT_A, pipeB, B, inB, IN_B, pipeC, C, pipeD, D, f_pid, F_Pid, loc_A, Loc_A, flowmeterpid, FlowMeterInst_Pid}.

%get_data() ->
%  ResourceInstances = lists:reverse(digitaltwin_server:get_ResourceInstances()),
%  #{resource_data => lists:reverse(get_data(ResourceInstances))}.
%get_data([]) -> [];
%get_data([ResInst]) ->
%  [#{num => 1, type => element(1, ResInst)}];
%get_data(ResourceInstances) ->
%  [FirstResInst | _] = ResourceInstances,
%  get_data(ResourceInstances, FirstResInst, [], FirstResInst).
%get_data(ResourceInstances, ResInst, [], FirstResInst) ->
%  {ok, [_ConnIN, ConnOUT]} = resource_instance:list_connectors(element(2, ResInst)),
%  Connected = connector:get_connected(ConnOUT),
%  case Connected of
%    {error, _, _, _, _} ->
%      [#{num => 1, type => element(1, ResInst)}];
%    {ok, disconnected} -> [#{num => 1, type => element(1, ResInst)}];
%    _Else ->
%      {ok, NextResInst_Pid} = connector:get_ResInst(element(2, Connected)),
%      NextResInst = lists:keyfind(NextResInst_Pid, 2, [spawn || ResourceInstances]),
%      if
%        NextResInst =:= false -> [#{num => 1, type => element(1, ResInst)}];
%        true -> get_data(ResourceInstances, NextResInst, [#{num => 1, type => element(1, ResInst)}], FirstResInst)
%      end
%  end;
%get_data(ResourceInstances, ResInst, Result, FirstResInst) ->
%  [Prev_Result | _] = Result,
%  {ok, [_ConnIN, ConnOUT]} = resource_instance:list_connectors(element(2, ResInst)),
%  Connected = connector:get_connected(ConnOUT),
%  case Connected of
%    {error, _, _, _, _} ->
%      [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result];
%    {ok, disconnected} -> [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result];
%    _Else ->
%      {ok, NextResInst_Pid} = connector:get_ResInst(element(2, Connected)),
%      NextResInst = lists:keyfind(NextResInst_Pid, 2, ResourceInstances),
%      if
%        NextResInst =:= false -> [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result];
%        NextResInst =:= FirstResInst -> [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result];
%        true -> get_data(ResourceInstances, NextResInst, [#{num => maps:get(num, Prev_Result)+1, type => element(1, ResInst)} | Result], FirstResInst)
%      end
%  end.
