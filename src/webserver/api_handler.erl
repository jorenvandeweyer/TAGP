-module(api_handler).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Path = cowboy_req:path_info(Req0),

	Req = handler(Path, Method, HasBody, Req0),
	{ok, Req, Opts}.

handler([<<"observer">> | _], <<"POST">>, _, Req) ->
	observer:start(),
	request:reply(json, ok, Req);

handler([<<"test">> | _], <<"POST">>, _, Req) ->
	digitaltwin:test(),
	request:reply(json, ok, Req);

handler([<<"system">> | _], <<"POST">>, true, Req) ->
	{ok, Body, Req0} = cowboy_req:read_body(Req),
	Type = request:fromJson(Body, type),
	digitaltwin:add_resource(Type),
	request:reply(json, ok, Req0);
handler([<<"system">> | _], <<"GET">>, _, Req) ->
	Data = digitaltwin:get_data(),
	Content = jiffy:encode(Data),
	request:reply(json, Content, Req);

% handler([<<"pump">>, Pid, State | _], <<"POST">>, _, Req) ->


handler([<<"instance">>, PidString | _], <<"GET">>, _, Req) ->
	Pid = convert:bin_to_pid(PidString),
	Data = digitaltwin:get_resource_data(Pid),
	Content = jiffy:encode(Data),
 	request:reply(json, Content, Req);
handler([<<"instance">>, PidString | _], <<"POST">>, _, Req) ->
	{ok, Body, Req0} = cowboy_req:read_body(Req),
	Pid = convert:bin_to_pid(PidString),
	On = request:fromJsonBool(Body, on),
	if 
		On -> 
			pumpInst:switch_on(Pid);
		true ->
			pumpInst:switch_off(Pid)	
	end,
	request:reply(json, ok, Req0);
handler([<<"dupbit">> | Path ], <<"GET">>, _, Req) ->
	PartUrl = string:join([binary_to_list(X) || X <- Path], "/"),
	Content = request:post("https://dupbit.com/api/" ++ PartUrl, {[
		{<<"username">>, <<"joren">>}
	]}),
	request:reply(json, Content, Req);
handler([<<"ip">> | _], <<"GET">>, _, Req) ->
	Content = request:get("https://dupbit.com/api/ip"),
	request:reply(text, Content, Req);
handler(_, _, _, Req) ->
	request:reply(json, error, Req).
