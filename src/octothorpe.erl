-module (octothorpe).

-export ([start/0]).

start() ->
    ssl:start(),
    application:start(compiler),
    application:start(syntax_tools),
    application:start(goldrush),
    application:start(crypto),
    application:start(ranch),
    application:start(cowlib),
    application:start(cowboy),
    application:start(jsx),
    ok = application:start(octothorpe).

