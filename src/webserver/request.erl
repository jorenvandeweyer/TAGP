-module(request).
-export([get/1, get/2, get/3, post/2, post/3]).
-export([reply/3]).

get(Url) ->
    {ok, {_Status, _Headers, Content}} = httpc:request(get, {
        Url, []
    }, [], []),
    Content.

get(Url, Data) ->
    get(json, Url, Data).

get(json, Url, Data) ->
    Json = jiffy:encode(Data),
    {ok, {_Status, _Headers, Content}} = httpc:request(get, {
        Url, [],
        "application/json",
        Json
    }, [], []),
    Content.

post(json, Url, Data) ->
    Json = jiffy:encode(Data),
    {ok, {_Status, _Headers, Content}} = httpc:request(post, {
        Url, [],
        "application/json",
        Json
    }, [], []),
    Content.

post(Url, Data) ->
    post(json, Url, Data).

reply(json, Json, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Json, Req);
reply(text, Text, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, Text, Req).
