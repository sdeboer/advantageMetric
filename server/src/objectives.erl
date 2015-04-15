-module(objectives).

-behaviour(gen_server).

-export([ start_link/0 ]).

% client API
-export([
				 times/1,
				 streaks/1
				]).

% gen_server API
-export([
				 init/1, terminate/2,
				 handle_info/2, handle_call/3, handle_cast/2,
				 code_change/3
				]).

-record(streak, {
					team,
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

% Amount of time that needs to lapse before its another streak
-define(STREAK_GAP, 0.33).
-define(URF_STREAK_GAP, ?STREAK_GAP / 2).

% Amount of time before or after a streak that someone can be applying
% positional pressure
-define(PRESSURE_GAP, ?STREAK_GAP / 2).
% And the radius they need to be in order to be considered pressuring
% the streak
-define(PRESSURE_RADIUS, 1500).

-define(BLUE_TEAM, 100).
-define(RED_TEAM, 200).

% Worth Assesment
-define(WARD_POINTS, 1).
-define(WARD_KILL_POINTS, 2).
-define(CHAMPION_POINTS, 3).
-define(DRAGON_POINTS, 3).
-define(BARON_POINTS, 5).
-define(BUILDING_POINTS, 10).

-define(MIN(A), A / 1000 / 60).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

times(Match) ->
	gen_server:call(?MODULE, {process, Match}).

streaks(Match) ->
	gen_server:call(?MODULE, {streaks, Match}).

init(_Args) -> {ok, undefined}.

handle_call({process, Match}, _F, S) ->
	{reply, process(Match), S};

handle_call({streaks, Match}, _F, S) ->
	{reply, get_streaks(Match), S}.

get_streaks(M) ->
	Timeline = proplists:get_value(<<"timeline">>, M),
	Frames = proplists:get_value(<<"frames">>, Timeline, []),
	Times = lists:foldl(fun process_frame/2, [], Frames),
	T2 = lists:sort(fun(A,B) ->
											A#time.stamp =< B#time.stamp
									end, Times),
	Participants = proplists:get_value(<<"participants">>, M),
	{Atimes, Btimes} = lists:partition(fun(T) ->
																				 case T#time.team of
																					 undefined ->
																						 Pid = lists:nth(1, T#time.attackers),
																						 partition_events(Pid, Participants);

																					 Tid ->
																						 Tid =:= ?BLUE_TEAM
																				 end
																		 end, T2),

	Astreaks = lists:foldl(
							 fun(S, Acc)->
									 find_streaks(S, Acc, ?BLUE_TEAM)
									 end, [], Atimes),
	As1 = lists:filtermap(fun streak_clean/1, Astreaks),
	As2 = lists:sort(fun streak_sort/2, As1),

	Bstreaks = lists:foldl(
							 fun(S, Acc)->
									 find_streaks(S, Acc, ?RED_TEAM)
									 end, [], Btimes),
	Bs1 = lists:filtermap(fun streak_clean/1, Bstreaks),
	Bs2 = lists:sort(fun streak_sort/2, Bs1),
	[Frames, As2, Bs2].

partition_events(Pid, [P | Rest]) ->
	case proplists:get_value(<<"participantId">>, P) of
		Pid ->
			proplists:get_value(<<"teamId">>, P, undefined) =:= ?BLUE_TEAM;
		_Pid ->
			partition_events(Pid, Rest)
	end.

process(M) ->
	[Frames, Blue, Red] = get_streaks(M),
	Players = proplists:get_value(<<"participantIdentities">>, M),
	Participants = proplists:get_value(<<"participants">>, M),

	Scores = [ process_player(P) || P <- Players ],
	S2 = [ add_teams(Scores, P) || P <- Participants ],
	TeamLookup = lists:map(
								 fun(P) ->
										 { proplists:get_value(pid, P),
											 proplists:get_value(team, P) }
								 end, S2),

	Blue2 = add_pressuring_players(Frames, TeamLookup, Blue),
	Red2 = add_pressuring_players(Frames, TeamLookup, Red),
	[Blue2, Red2].

add_pressuring_players(Frames, TeamLookup, Streaks) ->
	lists:map(fun(S) ->
								S2 = streak_position(Frames, TeamLookup, S),
								S2#streak{
									players = lists:usort(
															lists:flatten(S#streak.players)
														 )
								 }
						end, Streaks).

get_participants(Frame) ->
	P1 = proplists:get_value(<<"participantFrames">>, Frame),
	lists:map(
		fun({_K, V}) -> V end,
		P1).

streak_position([], _, S) -> S;

streak_position([F | Rest], TeamLookup, S) ->
	T1 = S#streak.start - ?PRESSURE_GAP,
	T2 = S#streak.finish + ?PRESSURE_GAP,
	FT = proplists:get_value(<<"timestamp">>, F),
	Plist = if
						(T1 =< FT) and (FT =< T2) ->

							Pf = get_participants(F),
							%lager:debug("PF ~p ~p", [length(Pf), Pf]),
							PosPlayers = lists:filtermap(
														 fun(Participant) ->
																 Pid = proplists:get_value(<<"participantId">>, Participant),
																 Pos = proplists:get_value(<<"position">>, Participant),
																 lager:debug("Pos ~p ", [S#streak.positions]),
																 C1 = S#streak.team =:= proplists:get_value(Pid, TeamLookup),
																 C2 = lists:any(
																				fun(Spos)->
																						is_local(Spos, Pos)
																				end, S#streak.positions),
																 if
																	 C1 and C2 -> {true, Pid};
																	 true -> false
																 end
														 end, Pf),
							[ PosPlayers | S#streak.players];

						true -> S#streak.players
					end,
	S2 = S#streak{players = Plist},
	streak_position(Rest, TeamLookup, S2).

streak_clean(S) ->
	if
		length(S#streak.events) == 1 -> false;
		true ->
			P = lists:filter( fun(X) -> X =/= 0 end,
												lists:usort(
													lists:flatten(S#streak.players)
												 )
											),
			S2 = S#streak{
						 players = P,
						 positions = lists:filter( fun(X) -> X =/= undefined end, S#streak.positions)
						},
			{true, S2}
	end.

streak_sort(A, B) ->
	A#streak.start =< B#streak.start.

find_streaks(T, [], Team)->
	[#streak{
			team = Team,
			start = T#time.stamp,
			finish = T#time.stamp,
			positions = [T#time.position],
			events = [T#time.type],
			score = T#time.score,
			players = T#time.attackers
		 }];

find_streaks(T, [S | Rest], Team) ->
	New = T#time.stamp,
	Diff = abs(New - S#streak.finish),
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
			Snew = #streak{
								team = Team,
								score = T#time.score,
								start = T#time.stamp,
								finish = T#time.stamp,
								events = [T#time.type],
								positions = [T#time.position],
								players = T#time.attackers
							 },
			[Snew, S | Rest]
	end.

is_local(A, B) ->
	X1 = proplists:get_value(<<"x">>, A),
	Y1 = proplists:get_value(<<"y">>, A),
	X2 = proplists:get_value(<<"x">>, B),
	Y2 = proplists:get_value(<<"y">>, B),
	D = math:sqrt(
				math:pow( X2 - X1, 2) + math:pow( Y2 - Y1, 2)
			 ),
	D =< ?PRESSURE_RADIUS.

process_player(Pdata) ->
	Player = proplists:get_value(<<"player">>, Pdata),
	[
	 { pid, proplists:get_value(<<"participantId">>, Pdata)},
	 { sid, proplists:get_value(<<"summonerId">>, Player)},
	 { name, proplists:get_value(<<"summonerName">>, Player)},
	 { score, 0 },
	 { position, undefined }
	].

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
							#time{type = ward_placed, score = ?WARD_POINTS, attackers = [Creator]};

						<<"WARD_KILL">> -> 
							#time{type = ward_kill, score = ?WARD_KILL_POINTS, attackers = [Killer]};

						<<"CHAMPION_KILL">> -> 
							Victim = proplists:get_value(<<"victimId">>, E),
							#time{type = champion, score = ?CHAMPION_POINTS, attackers = [Killer | Assists], victims = [Victim]};

						<<"ELITE_MONSTER_KILL">> -> 
							case proplists:get_value(<<"monsterType">>, E, undefined) of
								<<"BARON_NASHOR">> ->
									#time{type = baron, score = ?BARON_POINTS, attackers = [Killer | Assists]};
								<<"DRAGON">> ->
									#time{type = dragon, score = ?DRAGON_POINTS, attackers = [Killer | Assists]};
								_ -> undefined
							end;

						<<"BUILDING_KILL">> -> 
							% The team of the building is the owning team rather than the
							% attacking team, we'll just reverse that for our purposes.
							Team = case proplists:get_value(<<"teamId">>, E) of
											 100 -> 200;
											 200 -> 100
										 end,
							#time{type = building, score = ?BUILDING_POINTS, attackers = [Killer | Assists], team = Team};

						_ -> undefined
					end,

	case Score of
		undefined -> Acc;

		S ->
			S2 = S#time{stamp = ?MIN(proplists:get_value(<<"timestamp">>, E)),
									position = proplists:get_value(<<"position">>, E)},
			[S2 | Acc]
	end.

handle_cast(_R, _S) -> {stop, bad_cast}.

handle_info(_R, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVersion, S, _Extra) -> {ok, S}.

