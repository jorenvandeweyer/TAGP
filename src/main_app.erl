%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(main_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
  digitaltwin:start_link(),

	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, main, "html/index.html"}},
			{"/api/[...]", api_handler, []},
			{"/assets/[...]", cowboy_static, {priv_dir, main, "/assets"}},
			{"/[...]", cowboy_static, {priv_dir, main, "/html"}}
		]}
	]),

  {ok, _} = cowboy:start_clear(my_http_listener, [{port, 8443}], #{
    env => #{dispatch => Dispatch}
  }),

	%privDir = code:priv_dir(main),
	%ok, _} = cowboy:start_tls(https, [
	%	{port, 8443},
	%	{certfile, PrivDir ++ "/ssl/fullchain.pem"},
	%	{keyfile, PrivDir ++ "/ssl/privkey.pem"}
	%, #{
	%	env => #{dispatch => Dispatch}
	%),

	main_sup:start_link().

stop(_State) ->
	ok.
