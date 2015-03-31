-module(cors).

-behaviour(cowboy_middleware).

-export([execute/2]).

execute(Req, Env) ->
	Origin = allow_origin(),
	R2 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Methods">>, <<"GET, POST, OPTIONS">>, Req),
	R3 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Origin">>, Origin, R2),
	R4 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Headers">>, <<"Content-Type, Authorization, X-Requested-With, Accept, Origin">>, R3),
	R5 = cowboy_req:set_resp_header(<<"Access-Control-Max-Age">>, <<"1000">>, R4),
	R6 = cowboy_req:set_resp_header(<<"Access-Control-Allow-Credentials">>, <<"true">>, R5),

	case cowboy_req:method(Req) of
		{<<"OPTIONS">>, _} -> {halt, R6};
		_M -> {ok, R6, Env}
	end.

allow_origin() ->
	% And this could probably be DRY'd into the advantage_server_app
	Dom = case os:getenv("CLIENT_DOMAIN") of
		false ->
			{ok, App} = application:get_application(whereis(advantage_server_sup)),
			{ok, D2} = application:get_env(App, advantage_client_domain),
			D2;
		D1 -> D1
	end,
	[<<"http://">>, Dom].
