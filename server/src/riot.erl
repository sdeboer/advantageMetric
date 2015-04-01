-module(riot).

-behaviour(gen_server).

-export([ start_link/0 ]).

% client API
-export([
				 summoner_id/1,
				 matches_for_name/1,
				 matches/1,
				 match/1
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

summoner_id(Name) ->
	gen_server:call(?MODULE, {summoner, Name}).

matches_for_name(Name) -> matches( summoner_id(Name) ).

matches(Id) ->
	gen_server:call(?MODULE, {matches, Id}).

match(Id) -> match(Id, false).

match(Id, Timeline) ->
	gen_server:call(?MODULE, {match, Id, Timeline}).

handle_call({summoner, Name}, _F, S) ->
	U = url("summoner/by-name", "v1.4", Name, S),
	R = request(U),
	{reply, R, S};

handle_call({matches, Id}, _F, S) ->
	U = url("matchhistory", "v2.2", Id, S),
	R = request(U),
	{reply, R, S};

handle_call({match, Id, Timeline}, _F, S) ->
	P = case Timeline of
				true -> [{"includeTimeline", "true"}];
				_ -> []
			end,
	U = url("match", "v2.2", Id, P, S),
	R = request(U),
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

