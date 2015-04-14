-module(objectives).

-behaviour(gen_server).

-export([ start_link/0 ]).

% client API
-export([
				 times/1,
				 times_match/1, streaks/1
				]).

% gen_server API
-export([
				 init/1, terminate/2,
				 handle_info/2, handle_call/3, handle_cast/2,
				 code_change/3
				]).

-record(streak, {
					start,
					finish,
					score = 0,
					events = [],
					players = [],
					positions = []
				 }).

-record(time, {
					stamp,
					position,
					type,
					score,
					team = undefined,
					attackers = [],
					victims = []
				 }).

-define(STREAK_GAP, 20000).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

times_match(Match) ->
	gen_server:call(?MODULE, {process, Match}).

streaks(Match) ->
	gen_server:call(?MODULE, {streaks, Match}).

times(Mid) ->
	gen_server:call(?MODULE, {times, Mid}).

init(_Args) -> {ok, undefined}.

handle_call({times, Mid}, _F, S) -> 
	case riot:match(Mid, true) of
		{error, C, V} -> {reply, {error, C, V}, S};
		M -> {reply, process(M), S}
	end;

handle_call({process, Match}, _F, S) ->
	{reply, process(Match), S};

handle_call({streaks, Match}, _F, S) ->
	{reply, get_streaks(Match), S}.

get_streaks(M) ->
	Timeline = proplists:get_value(<<"timeline">>, M),
	Frames = proplists:get_value(<<"frames">>, Timeline, []),
	Times = lists:foldl(fun process_frame/2, [], Frames),
	Participants = proplists:get_value(<<"participants">>, M),
	{Atimes, Btimes} = lists:partition(fun(T) ->
																				 case T#time.team of
																					 undefined ->
																						 Pid = lists:nth(1, T#time.attackers),
																						 partition_events(Pid, Participants);

																					 Tid ->
																						 Tid =:= 100
																				 end
																		 end, Times),

	Astreaks = lists:foldl(fun find_streaks/2, [], Atimes),
	Bstreaks = lists:foldl(fun find_streaks/2, [], Btimes),
	[12, Astreaks, Bstreaks].

partition_events(Pid, [P | Rest]) ->
	case proplists:get_value(<<"participantId">>, P) of
		Pid ->
			proplists:get_value(<<"teamId">>, P, undefined) =:= 100;
		_Pid ->
			partition_events(Pid, Rest)
	end.

process(M) ->
	[Frames, Streaks] = get_streaks(M),
	Players = proplists:get_value(<<"participantIdentities">>, M),
	Participants = proplists:get_value(<<"participants">>, M),

	Scores = [ process_player(P) || P <- Players ],
	S2 = [ add_teams(Scores, P) || P <- Participants ],

	add_scores(Frames, Streaks, S2),
	Streaks.

find_streaks(T, [])->
	[#streak{
			start = T#time.stamp,
			finish = T#time.stamp,
			positions = [T#time.position],
			events = [T#time.type],
			score = T#time.score,
			players = T#time.attackers
		 }];

find_streaks(T, [S | Rest])->
	New = T#time.stamp,
	Diff = abs(New - S#streak.finish),
	lager:debug("STREAK ~p, ~p, ~p", [S#streak.finish, New, New - S#streak.finish]),
	if
		Diff < ?STREAK_GAP ->


			S2 = S#streak{
						 finish = New,
						 positions = [T#time.position | S#streak.positions],
						 players = [T#time.attackers | S#streak.players],
						 events = [T#time.type | S#streak.events],
						 score = T#time.score + S#streak.score
						},
			[S2 | Rest];

		true ->
			Sprev = S#streak{
								players = lists:usort( lists:flatten(S#streak.players) )
							 },
			Snew = S#streak{
							 start = T#time.stamp,
							 finish = T#time.stamp,
							 events = [T#time.type],
							 positions = [T#time.position],
							 players = T#time.attackers
							},
			[Snew, Sprev | Rest]
	end.

add_scores([], _, Scores) -> Scores;
add_scores(_, [], Scores) -> Scores;

add_scores([A, B | Frest], [T | Trest], Scores) ->

	if
		T#time.stamp > B#time.stamp -> add_scores([B, Frest], [T | Trest], Scores);

		true ->
			Origin = T#time.position,
			Attackers = T#time.attackers,
			Apid = lists:nth(1, Attackers),

			P = proplists:get_value(<<"particpantFrames">>, A, []),
			_A2 = lists:foldl(fun(E, Acc) ->
														Pid = proplists:get_value(<<"participantID">>, E),
														case lists:member(Acc, Pid) of
															true -> Acc;
															false ->
																case is_same_team(Pid, Apid, Scores) of
																	false -> Acc;
																	true ->
																		Pos = proplists:get_value(<<"position">>, E),
																		case is_local(Pos, Origin) of
																			true -> [Pid | Acc];
																			false -> Acc
																		end
																end
														end
												end, Attackers, P)
	end,

	add_scores([A, B | Frest], [T | Trest], Scores).

is_local(A, B) -> A =:= B.

process_player(Pdata) ->
	Player = proplists:get_value(<<"player">>, Pdata),
	[
	 { pid, proplists:get_value(<<"participantId">>, Pdata)},
	 { sid, proplists:get_value(<<"summonerId">>, Player)},
	 { name, proplists:get_value(<<"summonerName">>, Player)},
	 { score, 0 },
	 { position, undefined }
	].

is_same_team(A, B, Scores) ->
	participant_team(A, Scores) =:= participant_team(B, Scores).

participant_team(Pid, [H | T]) ->
	case proplists:get_value(pid, H) of
		Pid ->
			proplists:get_value(team, H);
		_ ->
			participant_team(Pid, T)
	end.

add_teams(Scores, P) ->
	Team = proplists:get_value(<<"teamId">>, P),
	Pid = proplists:get_value(<<"participantId">>, P),

	lists:filtermap(fun (E) ->
											case proplists:get_value(pid, E) of
												Pid -> {true, [{team, Team} | E]};

												_ -> true
											end
									end, Scores).

process_frame(Frame, Acc) ->
	case proplists:get_value(<<"events">>, Frame, undefined) of
		undefined -> Acc;
		Events ->
			lists:foldl(fun(E, A) -> process_event(E, A) end, Acc, Events)
	end.

process_event(E, Acc) ->
	Assists = proplists:get_value(<<"assistingParticipantsId">>, E, []),
	Killer = proplists:get_value(<<"killerId">>, E, undefined),

	Score = case proplists:get_value(<<"eventType">>, E, undefined) of
						<<"WARD_PLACED">> -> 
							Creator = proplists:get_value(<<"creatorId">>, E),
							#time{type = ward_placed, score = 1, attackers = [Creator]};

						<<"WARD_KILL">> -> 
							#time{type = ward_kill, score = 2, attackers = [Killer]};

						<<"CHAMPION_KILL">> -> 
							Victim = proplists:get_value(<<"victimId">>, E),
							#time{type = champion, score = 3, attackers = [Killer | Assists], victims = [Victim]};

						<<"ELITE_MONSTER_KILL">> -> 
							case proplists:get_value(<<"monsterType">>, E, undefined) of
								<<"BARON_NASHOR">> ->
									#time{type = baron, score = 5, attackers = [Killer | Assists]};
								<<"DRAGON">> ->
									#time{type = dragon, score = 3, attackers = [Killer | Assists]};  % unless its the 5th dragon..maybe
								_ -> undefined
							end;

						<<"BUILDING_KILL">> -> 
							Team = proplists:get_value(<<"teamId">>, E),
							#time{type = building, score = 10, attackers = [Killer | Assists], team = Team};

						_ -> undefined
					end,

	case Score of
		undefined -> Acc;

		S ->
			lager:debug("attacker ~p / ~p", [S#time.type, S#time.attackers]),
			S2 = S#time{stamp = proplists:get_value(<<"timestamp">>, E),
									position = proplists:get_value(<<"position">>, E)},
			[S2 | Acc]
	end.

%case Score of
%undefined -> Acc;
%S -> 
%%Prime = proplists:get_value(<<"participantId">>, E, undefined),
%Pframes = proplists:get_value(<<"participantFrames">>, Frame, []),
%Ps = lists:foldl(fun(Part, Res) -> process_participant(Part, Res) end, [], Pframes),
%
%S2 = [ {time, proplists:get_value(<<"timestamp">>, E)},
%{position, proplists:get_value(<<"position">>, E, undefined) },
%{participants, Ps}
%| S ],
%[ S2 | Acc]
%end.

%process_participant(undefined, Acc) -> Acc;
%process_participant({Id, Details}, Acc) ->
%[{id, Id}, {position, proplists:get_value(<<"position">>, Details, undefined)} | Acc].

handle_cast(_R, _S) -> {stop, bad_cast}.

handle_info(_R, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVersion, S, _Extra) -> {ok, S}.

