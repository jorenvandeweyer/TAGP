-module(api_handler).

-export([init/2]).

init(Req0, Opts) ->
	Method = cowboy_req:method(Req0),
	HasBody = cowboy_req:has_body(Req0),
	Path = cowboy_req:path_info(Req0),

	Req = handler(Path, Method, HasBody, Req0),
	{ok, Req, Opts}.

handler([<<"data">> | _], <<"GET">>, _, Req) ->
	Content0 = digitaltwin:get_data(),
	Content = jiffy:encode(Content0),
	request:reply(json, Content, Req);
handler([<<"pipes">> | _], <<"GET">>, _, Req) ->
	Content0 = digitaltwin:get_data(),
	Content = jiffy:encode(Content0),
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
