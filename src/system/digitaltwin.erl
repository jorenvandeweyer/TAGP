-module(digitaltwin).

-export([start_link/0, test/0]).
-export([init/0, init_resources/0, get_data/0, add_resource/1, get_resource_data/1]).

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
  case ResourceInstances of
    [] -> [];
    [ResInst| _] ->
      {ok, [IN, _OUT]} = resource_instance:list_connectors(element(2,ResInst)),
      {ok, {_, Connectors}} = fluidumTyp:discover_circuit(IN),
      get_data(maps:keys(Connectors), ResourceInstances, [])
  end.

get_data([], _ResourceInstances, Result) ->
  Result;
get_data([Connector | Remainder], ResourceInstances, Result) ->
  Res = lists:foldl(fun(N, ResInst) ->
                      {ok, [IN, OUT]} = resource_instance:list_connectors(element(2,N)),
                      if
                        (IN =:= Connector) or (OUT =:= Connector) ->
                          N;
                        true -> ResInst
                      end
                    end, no_ResInst, ResourceInstances),
  case lists:member(Res, Result) of
    true -> get_data(Remainder, ResourceInstances, Result);
    false -> get_data(Remainder, ResourceInstances, [#{type=>element(1,Res), pid=>convert:pid_to_bin(element(2,Res)) } | Result])
  end.

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

make_flowMeter(RealWorldCmdFn) ->
  PipeTyp_Pid = digitaltwin_server:get_ResourceType(pipeTyp),
  {ok, PipeInst_Pid} = resource_instance:create(pipeInst, [digitaltwin, PipeTyp_Pid]),
  FlowMeterTyp_Pid = digitaltwin_server:get_ResourceType(flowMeterTyp),
  {ok, FlowMeterInst_Pid} = resource_instance:create(flowMeterInst, [digitaltwin, FlowMeterTyp_Pid, PipeInst_Pid, RealWorldCmdFn]),
  digitaltwin_server:add_ResourceInstance(flowMeterInst, FlowMeterInst_Pid),
  {ok, FlowMeterInst_Pid}.

make_heatExchanger() ->
  PipeTyp_Pid = digitaltwin_server:get_ResourceType(pipeTyp),
  {ok, PipeInst_Pid} = resource_instance:create(pipeInst, [digitaltwin, PipeTyp_Pid]),
  HeatExchangerTyp_Pid = digitaltwin_server:get_ResourceType(heatExchangerTyp),
  HE_link_spec = #{delta => 0.8},
  {ok, HeatExchangerInst_Pid} = resource_instance:create(heatExchangerInst, [digitaltwin, HeatExchangerTyp_Pid, PipeInst_Pid, HE_link_spec]),
  digitaltwin_server:add_ResourceInstance(heatExchangerInst, HeatExchangerInst_Pid),
  {ok, heatExchangerInst}.

make_pipe() ->
  PipeTyp_Pid = digitaltwin_server:get_ResourceType(pipeTyp),
  {ok, PipeInst_Pid} = resource_instance:create(pipeInst, [digitaltwin, PipeTyp_Pid]),
  digitaltwin_server:add_ResourceInstance(pipeInst, PipeInst_Pid),
  {ok, PipeInst_Pid}.

make_pump(RealWorldCmdFn) ->
  PipeTyp_Pid = digitaltwin_server:get_ResourceType(pipeTyp),
  {ok, PipeInst_Pid} = resource_instance:create(pipeInst, [digitaltwin, PipeTyp_Pid]),
  PumpTyp_Pid = digitaltwin_server:get_ResourceType(pumpTyp),
  {ok, PumpInst_Pid} = resource_instance:create(pumpInst, [digitaltwin, PumpTyp_Pid, PipeInst_Pid, RealWorldCmdFn]),
  digitaltwin_server:add_ResourceInstance(pumpInst, PumpInst_Pid),
  {ok, PumpInst_Pid}.

set_fluidum(Root_ConnectorPid) ->
  FluidumTyp_Pid = digitaltwin_server:get_ResourceType(fluidumTyp),
  {ok, FluidumInst_Pid} = resource_instance:create(fluidumInst, [Root_ConnectorPid, FluidumTyp_Pid]),
  digitaltwin_server:set_FluidumInstance(FluidumInst_Pid),
  {ok, FluidumInst_Pid}.

add_resource(flowMeter) ->
  {_, FirstResInst} = digitaltwin_server:get_FirstResourceInst(),
  {_, LastResInst} = digitaltwin_server:get_LastResourceInst(),
  {ok, FlowMeter_Pid} = make_flowMeter(fun(A) -> 2*A end),
  if
    (FirstResInst =:= []) or (LastResInst =:= []) -> {ok, FlowMeter_Pid};
    true -> connect_ResourceInstances([FirstResInst, FlowMeter_Pid, LastResInst])
  end;
add_resource(heatExchanger) ->
  {_, FirstResInst} = digitaltwin_server:get_FirstResourceInst(),
  {_, LastResInst} = digitaltwin_server:get_LastResourceInst(),
  {ok, HeatExchanger_Pid} = make_heatExchanger(),
  if
    (FirstResInst =:= []) or (LastResInst =:= []) -> {ok, HeatExchanger_Pid};
    true ->   connect_ResourceInstances([FirstResInst, HeatExchanger_Pid, LastResInst])
  end;
add_resource(pipe) ->
  {_, FirstResInst} = digitaltwin_server:get_FirstResourceInst(),
  {_, LastResInst} = digitaltwin_server:get_LastResourceInst(),
  {ok, Pipe_Pid} = make_pipe(),
  if
    (FirstResInst =:= []) or (LastResInst =:= []) -> {ok, Pipe_Pid};
    true ->    connect_ResourceInstances([FirstResInst, Pipe_Pid, LastResInst])
  end;
add_resource(pump) ->
  {_, FirstResInst} = digitaltwin_server:get_FirstResourceInst(),
  {_, LastResInst} = digitaltwin_server:get_LastResourceInst(),
  {ok, Pump_Pid} = make_pump(fun  (Flow, on) -> Flow*10;
                                  (Flow, off) -> 0
                             end),
  if
    (FirstResInst =:= []) or (LastResInst =:= []) -> {ok, Pump_Pid};
    true ->   connect_ResourceInstances([FirstResInst, Pump_Pid, LastResInst])
  end;

add_resource(removeLast) ->
  digitaltwin_server:remove_LastResourceInst(),
  {_, FirstResInst} = digitaltwin_server:get_FirstResourceInst(),
  {_, LastResInst} = digitaltwin_server:get_LastResourceInst(),
  if
    (FirstResInst =:= []) or (LastResInst =:= []) -> {ok, no_resources};
    true ->   connect_ResourceInstances([FirstResInst, LastResInst])
  end.

get_resource_data(ResInst_Pid) -> ok.


test() ->
  {ok, A} = make_pipe(), {ok, B} = make_pipe(), {ok, C} = make_pipe(), {ok, D} = make_pipe(),
  {ok, [IN_A, OUT_A]} = resource_instance:list_connectors(A),
  {ok, [IN_B, _OUT_B]} = resource_instance:list_connectors(B),
  {ok, F_Pid} = set_fluidum(IN_A),
  {ok, [Loc_A]} = resource_instance:list_locations(A),
  location:arrival(Loc_A, F_Pid),
  {ok, FlowMeterInst_Pid} = make_flowMeter(fun(Arg)-> Arg*2 end),
  connect_ResourceInstances([A, B, C, D, FlowMeterInst_Pid, A]),
  {pipeA, A, outA, OUT_A, pipeB, B, inB, IN_B, pipeC, C, pipeD, D, f_pid, F_Pid, loc_A, Loc_A, flowmeterpid, FlowMeterInst_Pid}.
