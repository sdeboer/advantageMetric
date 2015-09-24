-module(advantage).
-include("match_ids.hrl").

-behaviour(gen_server).

-export([ start_link/0 ]).

% client API
-export([
				 fetch/2
				]).

% gen_server API
-export([
				 init/1, terminate/2,
				 handle_info/2, handle_call/3, handle_cast/2,
				 code_change/3
				]).

fetch(Sid, Mid) ->
	gen_server:call(?MODULE, {fetch, Sid, Mid}).

handle_call({fetch, _Sid, Mid}, _F, S) ->
	Cid = Mid#match_ids.champion,
	Tid = Mid#match_ids.team,
	Mid = Mid#match_ids.match,

	M = riot:match(Mid, true),

	Parts = proplists:get_value(<<"participants">>, M),
	Pid = lookup_pid(Tid, Cid, Parts),

	Streaks = objectives:scores(M),

	Reply = [
	 {match, M},
	 {pid, Pid},
	 {streaks, Streaks}
	],

	{reply, Reply, S}.

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

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_Args) ->
	{ok, undefined}.

handle_cast(_R, _S) -> {stop, bad_cast}.

handle_info(_R, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVersion, S, _Extra) -> {ok, S}.

