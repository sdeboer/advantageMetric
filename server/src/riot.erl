-module(riot).

-behaviour(gen_server).

-export([ start_link/0 ]).

% client API
-export([
				 summoner_id/1,
				 ranked/1, games/1,
				 match/1, match/2
				]).

% gen_server API
-export([
				 init/1, terminate/2,
				 handle_info/2, handle_call/3, handle_cast/2,
				 code_change/3
				]).

-record(state, {
					api_key,
					region = "na"
				 }).

-define(API_KEY_ENV, "RIOT_API_KEY").
-define(PROTO, "https://").
-define(DOMAIN, ".api.pvp.net/").
-define(PATH, "api/lol/").
-define(LIMIT, 3).

summoner_id(Name) ->
	gen_server:call(?MODULE, {summoner, string:to_lower(Name)}).

ranked(Id) ->
	gen_server:call(?MODULE, {ranked, Id}).

games(Id) ->
	gen_server:call(?MODULE, {games, Id}).

match(Id) -> match(Id, false).

match(Id, Timeline) ->
	gen_server:call(?MODULE, {match, Id, Timeline}, 10000).

handle_call({summoner, Name}, _F, S) ->
	U = url("summoner/by-name", "v1.4", Name, S),
	N = list_to_binary(Name),

	R = case request(U) of
				{ok, [{N, Props}]} ->
					proplists:get_value(<<"id">>, Props);
				{error, C, V} -> {error, C, V}
			end,

	{reply, R, S};

handle_call({games, Id}, _F, S) ->
	Arg = lists:flatten( [ io_lib:format("~p", [Id]) | "/recent" ]),
	U = url("game/by-summoner", "v1.3", Arg, [{endIndex, 5}], S),

	R = case request(U) of
				{ok, Res} ->
					GL = proplists:get_value(<<"games">>, Res),
					GL2 = lists:sublist(lists:filter(
									fun(G) ->
											Gm = proplists:get_value(<<"gameMode">>, G),
											St = proplists:get_value(<<"subType">>, G),
											lager:debug("GT ~p / ~p", [Gm, St]),
											is_accepted_queue(Gm, St)
									end, GL), ?LIMIT),

					[ proplists:get_value(<<"gameId">>, X) || X <- GL2 ];
				{error, C, V} -> {error, C, V}
			end,

	{reply, R, S};

handle_call({ranked, Id}, _F, S) ->
	U = url("matchhistory", "v2.2", Id, [{endIndex, 5}], S),

	R = case request(U) of
				{ok, [{<<"matches">>, ML}]} ->
					ML2 = lists:sublist(lists:filter(
									fun(M) ->
											Qt = proplists:get_value(<<"queueType">>, M),
											lager:debug("QT ~p", [Qt]),
											is_accepted_queue(Qt)
									end, ML), ?LIMIT),

					[ proplists:get_value(<<"matchId">>, X) || X <- ML2 ];
				{error, C, V} -> {error, C, V}
			end,

	{reply, R, S};

handle_call({match, Id, Timeline}, _F, S) ->
	P = case Timeline of
				true -> [{"includeTimeline", "true"}];
				_ -> []
			end,
	U = url("match", "v2.2", Id, P, S),
	R = case request(U) of
				{ok, M} -> M;
				X -> X
			end,
	{reply, R, S}.

request(Url) ->
	case restc:request(get, Url) of
		{ok, 200, _H, V} -> {ok, V};
		{ok, C, _H, Data} ->
			S = proplists:get_value(<<"status">>, Data),
			M = proplists:get_value(<<"message">>, S),
			{error, C, M}
	end.

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	case os:getenv(?API_KEY_ENV) of
		false ->
			{stop, no_api_key};
		Key ->
			{ok, #state{api_key = Key}}
	end.

handle_cast(_R, _S) -> {stop, bad_cast}.

handle_info(_R, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVersion, S, _Extra) -> {ok, S}.

link(Region, Request, Version, Arg, Params) ->
	Base = lists:concat([?PROTO, Region, ?DOMAIN]),
	Path = lists:concat([?PATH, Region, "/", Version, "/", Request, "/", Arg]),
	restc:construct_url(Base, Path, Params).

url(Request, Version, Arg, Params, S) ->
	link(S#state.region, Request, Version, Arg, [{"api_key", S#state.api_key} | Params]).

url(Request, Version, Arg, S)->
	url(Request, Version, Arg, [], S).

is_accepted_queue(<<"CLASSIC">>, X) -> is_accepted_queue(X).

is_accepted_queue(<<"URF">>) -> true;
is_accepted_queue(<<"CAP_5x5">>) -> true;
is_accepted_queue(<<"NORMAL_5x5_BLIND">>) -> true;
is_accepted_queue(<<"RANKED_SOLO_5x5">>) -> true;
is_accepted_queue(<<"RANKED_PREMADE_5x5">>) -> true;
is_accepted_queue(<<"NORMAL_5x5_DRAFT">>) -> true;
is_accepted_queue(<<"RANKED_TEAM_5x5">>) -> true;
is_accepted_queue(<<"GROUP_FINDER_5x5">>) -> true;
is_accepted_queue(<<"SR_6x6">>) -> true;
is_accepted_queue(<<"URF_5x5">>) -> true;
is_accepted_queue(<<"HEXAKILL">>) -> true;
is_accepted_queue(_) -> false.
