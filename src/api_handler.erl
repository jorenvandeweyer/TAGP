-module(api_handler).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Path = cowboy_req:path_info(Req0),

	Req = handler(Path, Method, HasBody, Req0),
	{ok, Req, Opts}.

handler([<<"dupbit">> | Path ], <<"GET">>, _, Req) ->
	PartUrl = string:join([binary_to_list(X) || X <- Path], "/"),
	Content = request:post("https://dupbit.com/api/" ++ PartUrl, {[
		{<<"username">>, <<"joren">>}
	]}),
	request:reply(json, Content, Req);
handler([<<"ip">> | _], <<"GET">>, _, Req) ->
	Content = request:get("https://dupbit.com/api/ip"),
	request:reply(text, Content, Req);
handler([<<"pipe">> | _], <<"GET">>, _, Req) ->
	Content = jiffy:encode({[
		{<<"REST">>, <<"GET">>}, 
		{<<"PATH">>, <<"pipe">>}
	]}),
	request:reply(json, Content, Req);	
handler([<<"connectors">> | _], <<"GET">>, _, Req) ->
	Content = jiffy:encode({[
		{<<"REST">>, <<"GET">>}, 
		{<<"PATH">>, <<"connectors">>},
		{<<"IN">>, <<"0.192">>},
		{<<"OUT">>, <<"0.193">>}
	]}),
	request:reply(json, Content, Req);
handler(_, _, _, Req) ->
	request:reply(text, <<"boiiiii">>, Req).
