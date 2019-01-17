-module(request).
-export([get/1, get/2, get/3, post/2, post/3]).
-export([reply/3]).
-export([fromJson/2, fromJsonBool/2]).

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

reply(json, ok, Req) ->
	Content = jiffy:encode({[{status, true}]}),
    reply(json, Content, Req);
reply(json, error, Req) ->
    Content = jiffy:encode({[{status, false}]}),
    reply(json, Content, Req);
reply(json, Json, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"application/json">>
    }, Json, Req);
reply(text, Text, Req) ->
    cowboy_req:reply(200, #{
        <<"content-type">> => <<"text/plain">>
    }, Text, Req).

fromJson(Body, Key) ->
    {Json} = jiffy:decode(Body),
    KeyBin = erlang:atom_to_binary(Key, latin1),
    Type = proplists:get_value(KeyBin, Json),
    erlang:binary_to_atom(Type, latin1).
fromJsonBool(Body, Key) ->
    {Json} = jiffy:decode(Body),
    KeyBin = erlang:atom_to_binary(Key, latin1),
    Type = proplists:get_value(KeyBin, Json),
    Type.
