-module(octothorpe_app).
-behaviour(application).

-export([start/2]).
-export([stop/1]).

start(_Type, _Args) ->
    start_webserver(),
    octothorpe_sup:start_link().

stop(_State) ->
    ok.

start_webserver() ->
    Dispatch = cowboy_router:compile(
                 [
                  {'_', [
                         {"/", cowboy_static, {file, "./priv/index.html"}},
                         {"/alt", cowboy_static, {file, "./priv/index_ws.html"}},
                         {"/static/[...]", cowboy_static, {dir, "./priv/static"}},
                         {"/rest/play", octothorpe_rest, []},
                         {"/ws", octothorpe_ws, []}
                        ]}
                 ]),
    {ok, Port} = application:get_env(octothorpe, port),
    {ok, _} = cowboy:start_http(
                http,
                100,
                [{port, Port}],
                [
                 {env, [{dispatch, Dispatch}]}
                ]).
