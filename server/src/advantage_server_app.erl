-module(advantage_server_app).

-behaviour(application).
-export([start/2, stop/1]).

-define(C_ACCEPTORS, 5).

start(normal, _StartArgs) ->
	PortOpts = [{port, port()}],
	ProtoOpts = [
							 {env, [{dispatch, routes()}]},
							 {middlewares, [cowboy_router, cors, cowboy_handler]}
							],

	case cowboy:start_http(http, ?C_ACCEPTORS, PortOpts, ProtoOpts) of
		{ok, _CowboyPid} ->
			advantage_server_sup:start_link();
		{error, Report} ->
			lager:error("!! Critical error starting http : ~p", [Report]),
			% Ideally this should actually stop the whole thing....but
			% we want the error messages and returning anything but {ok, pid()}
			% here causes the badmatch error to occur before logs are written
			{ok, self()}
	end.

stop(_State) ->
	ok.

routes() ->
	cowboy_router:compile(
		[{'_',
			[
			 {"/summoner/:type/:summoner_name[/:compare_to_name]", summoner_handler, []},
			 {"/match/:match_id", match_handler, []}
			]
		 }]
	 ).

port() ->
	case os:getenv("PORT") of
		false ->
			{ok, Port2} = application:get_env(http_port),
			Port2;
		Port ->
			Port
	end.
