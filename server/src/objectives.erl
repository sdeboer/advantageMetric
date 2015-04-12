-module(objectives).

-behaviour(gen_server).

-export([ start_link/0 ]).

% client API
-export([
				 times/1,
				 times_match/1
				]).

% gen_server API
-export([
				 init/1, terminate/2,
				 handle_info/2, handle_call/3, handle_cast/2,
				 code_change/3
				]).

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

times_match(Match) ->
	gen_server:call(?MODULE, {process, Match}).

times(Mid) ->
	gen_server:call(?MODULE, {times, Mid}).

init(_Args) -> {ok, undefined}.

handle_call({times, Mid}, _F, S) -> 
	case riot:match(Mid, true) of
		{error, C, V} -> {reply, {error, C, V}, S};
		M -> {reply, process(M), S}
	end;

handle_call({process, Match}, _F, S) ->
	{reply, process(Match), S}.

process(M) ->
	Timeline = proplists:get_value(<<"timeline">>, M),
	Frames = proplists:get_value(<<"frames">>, Timeline, []),
	lists:foldl(fun(F, A) -> process_frame(F, A) end, [], Frames).

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
							[{type, ward_placed}, {score, 1}, {players, [Creator]}];

						<<"WARD_KILL">> -> 
							[{type, ward_kill}, {score, 2}, {players, [Killer]}];

						<<"CHAMPION_KILL">> -> 
							Victim = proplists:get_value(<<"victimId">>, E),
							[{type, champion}, {score, 3}, {players, [Killer, Victim | Assists] }];

						<<"ELITE_MONSTER_KILL">> -> 
							case proplists:get_value(<<"monsterType">>, E, undefined) of
								<<"BARON_NASHOR">> ->
									[{type, baron}, {score, 5}, {players, [Killer | Assists]}];
								<<"DRAGON">> ->
									[{type, dragon}, {score, 3}, {players, [Killer | Assists]}];  % unless its 5th then this should be 5
								_ -> undefined
							end;

						<<"BUILDING_KILL">> -> 
							[{type, building}, {score, 10}, {players, [Killer | Assists]}];

						_ -> undefined
					end,

	case Score of
		undefined -> Acc;

		S ->
			S2 = [ {time, proplists:get_value(<<"timestamp">>, E)},
						 {position, proplists:get_value(<<"position">>, E)} |
						 S ],
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

