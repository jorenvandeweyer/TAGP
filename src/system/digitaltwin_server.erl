-module(digitaltwin_server).
-behaviour(gen_server).

-export([start_link/0, stop/0, get_ResourceInstances/0, add_ResourceInstance/2, get_FluidumInstance/0,
    set_FluidumInstance/1, delete_AllResourceInstances/0, get_ResourceType/1, set_ResourceType/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
    terminate/2, code_change/3]).

start_link() ->
    gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast({global, ?MODULE}, terminate).

get_ResourceInstances() ->
    gen_server:call({global, ?MODULE}, {get_ResInst}).

get_FluidumInstance() ->
    gen_server:call({global, ?MODULE}, {get_FluidumInst}).

add_ResourceInstance(InstSelector, ResInst_Pid) ->
    gen_server:cast({global, ?MODULE}, {add_ResInst, InstSelector, ResInst_Pid}).

set_FluidumInstance(FluidumInst_Pid) ->
    gen_server:cast({global, ?MODULE}, {set_FluidumInst, FluidumInst_Pid}).

delete_AllResourceInstances() ->
    gen_server:cast({global, ?MODULE}, reset).

get_ResourceType(TypSelector) ->
    gen_server:call({global, ?MODULE}, {get_ResTyp, TypSelector}).

set_ResourceType(TypSelector, ResTyp_Pid) ->
    gen_server:cast({global, ?MODULE}, {set_ResTyp, TypSelector, ResTyp_Pid}).

init(_args) ->
    Data = {
            [], empty_fluidum,
            #{flowMeterTyp => empty, fluidumTyp => empty, heatExchangerTyp => empty,
            pipeTyp => empty, pumpTyp => empty}
    },

    {ok, Data}.

handle_cast(reset, Data) ->
    {noreply, {
        [], empty_fluidum,
        element(3, Data)
    }};
handle_cast({add_ResInst, InstSelector, ResInst_Pid}, Data) ->
    { ResInstances, FluidumInst, ResTypes} = Data,
    NewResInstances = if
        ResInstances =:= [] -> [{InstSelector, ResInst_Pid}];
        ResInstances =/= [] -> [{InstSelector, ResInst_Pid} | ResInstances]
    end,
    {noreply, {NewResInstances, FluidumInst, ResTypes}};
handle_cast({set_FluidumInst, FluidumInst_Pid}, Data) ->
    { ResInstances, _FluidumInst, ResTypes} = Data,
    {noreply, {ResInstances, FluidumInst_Pid, ResTypes}};
handle_cast({set_ResTyp, TypSelector, ResTyp_Pid}, Data) ->
    { ResInstances, FluidumInst, ResTypes} = Data,
    NewResTypes = ResTypes#{TypSelector := ResTyp_Pid},
    {noreply, {ResInstances, FluidumInst, NewResTypes}}.

handle_call({get_ResInst}, _From, Data) ->
    { ResInstances, _FluidumInst, _ResTypes} = Data,
    {reply, ResInstances, Data};
handle_call({get_FluidumInst}, _From, Data) ->
    { _ResInstances, FluidumInst, _ResTypes} = Data,
    {reply, FluidumInst, Data};
handle_call({get_ResTyp, TypSelector}, _From, Data) ->
    { _ResInstances, _FluidumInst, ResTypes} = Data,
    #{ TypSelector := ResData} = ResTypes,
    {reply, ResData, Data}.

handle_info(Info, Data) ->
    {noreply, Info, Data}.

terminate(_reason, _Data) ->
    io:format("terminating ~p~n", [{global, ?MODULE}]),
    ok.

code_change(_OldVersion, Data, _Extra) ->
    {ok, Data}.


    