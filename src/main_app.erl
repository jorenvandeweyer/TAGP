%% Feel free to use, reuse and abuse the code in this file.

%% @private
-module(main_app).
-behaviour(application).

%% API.
-export([start/2]).
-export([stop/1]).

%% API.

start(_Type, _Args) ->
	pipe_system:start_link(),
	pipe_system:create_pipe(),
	pipe_system:create_pipe(),
    pipe_system:create_pipe(),
    pipe_system:create_pipe(),
    pipe_system:connect_pipes(1, 2),
    pipe_system:connect_pipes(2, 3),
    pipe_system:connect_pipes(3, 4),
    pipe_system:connect_pipes(4, 1),
	Dispatch = cowboy_router:compile([
		{'_', [
			{"/", cowboy_static, {priv_file, main, "html/index.html"}},
			{"/api/[...]", api_handler, []},
			{"/assets/[...]", cowboy_static, {priv_dir, main, "/assets"}},
			{"/[...]", cowboy_static, {priv_dir, main, "/html"}}
		]}
	]),
	PrivDir = code:priv_dir(main),
	{ok, _} = cowboy:start_tls(https, [
		{port, 8443},
		{certfile, PrivDir ++ "/ssl/fullchain.pem"},
		{keyfile, PrivDir ++ "/ssl/privkey.pem"}		
	], #{
		env => #{dispatch => Dispatch}
	}),
	main_sup:start_link().

stop(_State) ->
	ok.
