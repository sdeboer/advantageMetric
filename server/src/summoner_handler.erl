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
-export([ scores/1 ]).

-record(state, {
					summoner_name,
					compare_to_name
				 }).

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
	Base = [{base, scores(S#state.summoner_name)}],
	Resp = case S#state.compare_to_name of
					 undefined -> Base;
					 Name -> [{compare, scores(Name)} | Base]
				 end,

	Json = jsx:encode(Resp),
	{B, R2} = json_handler:return_json(Json, Req),
	{B, R2, S}.

scores(Name) ->
	case riot:summoner_id(Name) of
		{error, C, V} -> {error, C, V};
		Sid ->
			Matches = riot:ranked(Sid),
			lists:map(
				fun(Mid) ->
						M = riot:match(Mid, true),
						Streaks = objectives:scores(M),
						[ {match, M}, 
						 {pid, match_pid(Sid, M)},
						 {streaks, Streaks}]
				end, Matches)
	end.

match_pid(Sid, Match) ->
	Idents = proplists:get_value(<<"participantIdentities">>, Match),
	match_participant(Sid, Idents).

match_participant(Sid, [P | Rest]) ->
	% this doesn't work for recent games, as we need to
	% get some data from the initial games call in order
	% to tell which champ the current player is using.
	Player = proplists:get_value(<<"player">>, P),
	case proplists:get_value(<<"summonerId">>, Player) of
		Sid ->
			proplists:get_value(<<"participantId">>, P);
		_ -> match_participant(Sid, Rest)
	end.
