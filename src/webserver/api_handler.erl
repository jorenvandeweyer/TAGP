-module(api_handler).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Path = cowboy_req:path_info(Req0),

	Req = handler(Path, Method, HasBody, Req0),
	{ok, Req, Opts}.

handler([<<"system">> | _], <<"POST">>, true, Req) ->
	{ok, Body, Req0} = cowboy_req:read_body(Req),
	Type = request:fromJson(Body, type),
	survivor:entry({type, Type}),
	digitaltwin:add_resource(Type),
	request:reply(json, ok, Req0);
handler([<<"system">> | _], <<"GET">>, _, Req) ->
	{ok, Data} = digitaltwin:get_data(),
	survivor:entry({get_data, Data}),
	Content = jiffy:encode(Data),
	request:reply(json, Content, Req);
handler([<<"dupbit">> | Path ], <<"GET">>, _, Req) ->
	PartUrl = string:join([binary_to_list(X) || X <- Path], "/"),
	Content = request:post("https://dupbit.com/api/" ++ PartUrl, {[
		{<<"username">>, <<"joren">>}
	]}),
	request:reply(json, Content, Req);
handler([<<"ip">> | _], <<"GET">>, _, Req) ->
	Content = request:get("https://dupbit.com/api/ip"),
	request:reply(text, Content, Req).
