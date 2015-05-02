-module(summoner_handler).

-export([init/3]).

% Rest Standards
-export([
				 rest_init/2,
				 content_types_provided/2
				]).

% Callbacks
-export([
				 to_json/2
				]).

% for testing
-export([ 
				 recent_scores/1,
				 ranked_scores/1
				]).

-record(state, {
					recent = true,
					summoner_name,
					compare_to_name
				 }).

-define(RANKED_QUEUE, "ranked").

init(_Transport, _Req, _Opts) ->
	{upgrade, protocol, cowboy_rest}.

rest_init(Req, _RouteOpts) ->
	case cowboy_req:binding(summoner_name, Req) of
		{undefined, R2} -> {stop, R2, undefined};

		{Name, R2} -> 
			S = #state{summoner_name = binary_to_list(Name)},
			case cowboy_req:binding(compare_to_name, R2) of
				{undefined, R3} -> {ok, R3, S};
				{Cname, R3} ->
					{ok, R3, S#state{compare_to_name = binary_to_list(Cname)}}
			end
	end.

content_types_provided(Req, S) ->
	{[
		{{<<"application">>, <<"json">>, '*'}, to_json},
		{<<"text/html">>, to_json }
	 ], Req, S}.

to_json(Req, S)->
	{Params, R2} = cowboy_req:parse_qs(Req),
	lager:debug("Recent? ~p", [ proplists:get_value(queue_type, Params) ] ),
	ScoreFn = case proplists:get_value(queue_type, Params) of
							?RANKED_QUEUE ->
								fun riot:ranked_ids/1;
							_ ->
								fun riot:recent_ids/1
						end,

	Summoner = [{summoner, retrieve(S#state.summoner_name, ScoreFn)}],

	Resp = case S#state.compare_to_name of
					 undefined -> Summoner;
					 Name -> [{compare, retrieve(Name, ScoreFn)} | Summoner]
				 end,

	Json = jsx:encode(Resp),
	{B, R3} = json_handler:return_json(Json, R2),
	{B, R3, S}.

recent_scores(Name) ->
	retrieve(Name, fun riot:recent_ids/1).

ranked_scores(Name) ->
	retrieve(Name, fun riot:ranked_ids/1).

retrieve(Name, Fn) ->
	case riot:summoner_id(Name) of
		{error, C, V} -> {error, C, V};
		Sid ->
			[ match_map(M) || M <- Fn(Sid) ]
	end.

match_map(Match) ->
	Cid = proplists:get_value(champion, Match),
	Tid = proplists:get_value(team, Match),
	Mid = proplists:get_value(matchId, Match),

	M = riot:match(Mid, true),

	Parts = proplists:get_value(<<"participants">>, M),
	Pid = lookup_pid(Tid, Cid, Parts),

	Streaks = objectives:scores(M),

	[
	 {match, M},
	 {pid, Pid},
	 {streaks, Streaks}
	].

lookup_pid(Tid, Cid, [P | Prest]) ->
	case proplists:get_value(<<"teamId">>, P) of
		Tid ->

			case proplists:get_value(<<"championId">>, P) of
				Cid ->

					proplists:get_value(<<"participantId">>, P);

				_ ->
					lookup_pid(Tid, Cid, Prest)
			end;

		_ ->
			lookup_pid(Tid, Cid, Prest)
	end.
