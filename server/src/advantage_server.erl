-module(advantage_server).

-export([start/0]).

start() ->
	ok = application:start(sync),
	ok = application:start(syntax_tools),
	ok = application:start(compiler),
	ok = application:start(goldrush),
	ok = application:start(lager),
	ok = application:start(crypto),
	ok = application:start(cowlib),
	ok = application:start(ranch),
	ok = application:start(cowboy),
	ok = application:start(inets),
	ok = application:start(asn1),
	ok = application:start(public_key),
	ok = application:start(ssl),
	ok = application:start(advantage_server),
	lager:info("advantage_server started"),
	ok.
