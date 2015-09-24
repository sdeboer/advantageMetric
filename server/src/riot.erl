-module(riot).
-include("match_ids.hrl").

-behaviour(gen_server).

-export([ start_link/0 ]).

% client API
-export([
				 summoner_id/1,
				 ranked_ids/1, recent_ids/1,
				 recent_list/1,
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

-define(MATCH_TYPE, <<"match">>).

summoner_id(Name) ->
	gen_server:call(?MODULE, {summoner, Name}).

ranked_ids(Id) ->
	gen_server:call(?MODULE, {ranked_ids, Id}).

recent_ids(Id) ->
	gen_server:call(?MODULE, {recent_ids, Id}).

recent_list(Id) ->
	gen_server:call(?MODULE, {recent_list, Id}).

match(Id) -> match(Id, false).

match(Id, Timeline) ->
	gen_server:call(?MODULE, {match, Id, Timeline}, 10000).

handle_call({summoner, Name}, _F, S) ->
	lager:info("Looking for ~p", [Name]),
	N = re:replace(
				string:to_lower(Name),
				"\\s+", "", [global,{return,list}]),
	N2 = http_uri:encode( N ),
	U = url("summoner/by-name", "v1.4", N2, S),
	N3 = list_to_binary(N),

	R = case request(U) of
				{ok, [{N3, Props}]} ->
					proplists:get_value(<<"id">>, Props);
				{error, C, V} -> {error, C, V}
			end,

	{reply, R, S};

handle_call({recent_list, Id}, _F, S) ->
	Arg = lists:flatten( io_lib:format("~p/recent", [Id]) ),
	U = url("game/by-summoner", "v1.3", Arg, [{endIndex, ?LIMIT}], S),
	case request(U) of
		{ok, Res} ->
			{reply, Res, S};
		{error, C, V} ->
			{reply,
			 {error, C, V},
			 S}
	end;

handle_call({recent_ids, Sid}, _F, S) ->
	Arg = lists:flatten( io_lib:format("~p/recent", [Sid]) ),
	U = url("game/by-summoner", "v1.3", Arg, [{endIndex, ?LIMIT}], S),

	R = case request(U) of
				{ok, Res} ->
					GL = proplists:get_value(<<"games">>, Res),
					lists:filtermap(
						fun(G) ->

								Gm = proplists:get_value(<<"gameMode">>, G),
								St = proplists:get_value(<<"subType">>, G),
								case is_accepted_queue(Gm, St) of
									true ->
										{true, game_team_champ(G)};
									false -> false
								end

						end, GL);

				{error, C, V} -> {error, C, V}
			end,

	{reply, R, S};

handle_call({ranked_ids, Sid}, _F, S) ->
	U = url("matchhistory", "v2.2", Sid, [{endIndex, ?LIMIT}], S),

	R = case request(U) of
				{ok, [{<<"matches">>, ML}]} ->
					lists:filtermap(
						fun(M) ->

								Qt = proplists:get_value(<<"queueType">>, M),
								case is_accepted_queue(Qt) of
									true ->
										{true, match_team_champ(Sid, M)};
									false -> false
								end

						end, ML);

				{error, C, V} -> {error, C, V}
			end,

	{reply, R, S};

handle_call({match, Id, Timeline}, _F, S) ->
	% Need to deal with whether or not we have the
	% timeline already.

	Bid = list_to_binary( integer_to_list( Id ) ),
	R = case document:load(?MATCH_TYPE, Bid) of
				undefined ->

					lager:debug("Retrieving from Riot ~p", [Id]),
					case retrieve_match(Id, Timeline, S) of
						{ok, M} ->
							document:create(?MATCH_TYPE, Bid, M);

						Err -> Err
					end;

				Match ->
					lager:debug("cached ~p", [Id]),
					Match
			end,

	{reply, R, S}.

retrieve_match(Id, Timeline, S) ->
	P = case Timeline of
				true -> [{"includeTimeline", "true"}];
				_ -> []
			end,
	U = url("match", "v2.2", Id, P, S),
	request(U).

request(Url) ->
	case restc:request(get, Url) of
		{ok, 200, _H, V} -> {ok, V};
		{ok, 404, _H, _D} -> {error, 404, <<"not found">>};
		{ok, C, _H, Data} ->
			% I think this actually needs to do a string
			% parse of the HTML returned in order to figure
			% out what the message is...It is not presented
			% as a proplist.
			M = case proplists:get_value(<<"status">>, Data) of
						undefined ->
							<<"no message">>;
						S ->
							proplists:get_value(<<"message">>, S)
					end,
			{error, C, M};
		{error, D} ->
			{error, [D], <<"Network connection issue.">>}
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

game_team_champ(Game) ->
	Tid = proplists:get_value(<<"teamId">>, Game),
	Cid = proplists:get_value(<<"championId">>, Game),
	Mid = proplists:get_value(<<"gameId">>, Game),
	#match_ids{
		 champion = Cid,
		 match = Mid,
		 team = Tid }.

match_team_champ(Sid, Match) ->
	Idents = proplists:get_value(<<"participantIdentities">>, Match),
	Pid = match_participant_id(Sid, Idents),

	P = match_participant(Pid, proplists:get_value(<<"participants">>, Match)),

	Tid = proplists:get_value(<<"teamId">>, P),
	Cid = proplists:get_value(<<"championId">>, P),
	Mid = proplists:get_value(<<"matchId">>, Match),
	#match_ids{
		 champion = Cid,
		 match = Mid,
		 team = Tid }.

match_participant(Pid, [P | Rest]) ->
	case proplists:get_value(<<"participantId">>, P) of
		Pid -> P;
		_ -> match_participant(Pid, Rest)
	end.

match_participant_id(Sid, [P | Rest]) ->
	Player = proplists:get_value(<<"player">>, P),
	case proplists:get_value(<<"summonerId">>, Player) of
		Sid ->
			proplists:get_value(<<"participantId">>, P);
		_ -> match_participant_id(Sid, Rest)
	end.

is_accepted_queue(<<"CLASSIC">>, X) -> is_accepted_queue(X);
is_accepted_queue(_, _) -> false.

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
