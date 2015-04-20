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
                         {"/", cowboy_static, {priv_file, octothorpe, "index.html"}},
                         {"/alt", cowboy_static, {priv_file, octothorpe, "index_ws.html"}},
                         {"/static/[...]", cowboy_static, {priv_dir, octothorpe, "static",
                                                    [{mimetypes, cow_mimetypes, all}]}},
                         {"/rest/play", octothorpe_rest, []},
                         {"/ws", octothorpe_ws, []}
                        ]}
                 ]),
    {ok, _} = cowboy:start_http(
                http,
                100,
                [{port, 8080}],
                [
                 {env, [{dispatch, Dispatch}]}
                ]).
