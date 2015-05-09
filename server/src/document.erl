-module(document).
-behaviour(gen_server).

-export([
				 start_link/0,
				 start_link/1]).

% gen_server API
-export([
				 init/1, terminate/2, stop/0, code_change/3,
				 handle_call/3, handle_cast/2, handle_info/2
				]).

% CRUD
-export([
				 id/0, id/1,
				 save/1,
				 get/1, get/2
				 %delete/1
				]).

% Views

-define(DEFAULT_URL, "http://localhost:5984").
-define(DEFAULT_DB, "advantage").

-record(state, {
					server,
					database
				 }).

id() -> id(1).

id(N) ->
	gen_server:call(?MODULE, {new_id, N}).

save(D) ->
	gen_server:call(?MODULE, {save, D}).

get(Id) ->
	get(Id, []).

get(Id, Args) ->
	gen_server:call(?MODULE, {retrieve, Id, Args}).

start_link() ->
	start_link([]).

% Can provide an options argument like this (defaults shown):
%
% [
% 	{url, "http://localhost:5984/"},
% 	{server_options, []},
% 	{database, "advantage"},
% 	{database_options, []}
% ]

start_link(Opts) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Opts, []).

init(Opts) ->
	Url = case proplists:get_value(url, Opts) of
					undefined -> ?DEFAULT_URL;
					U -> U
				end,
	Args = case proplists:get_value(server_options, Opts) of
					 undefined -> [];
					 O -> O
				 end,

	DB = case proplists:get_value(database, Opts) of
				 undefined -> ?DEFAULT_DB;
				 D -> D
			 end,

	DBargs = case proplists:get_value(database_options, Opts) of
						 undefined -> [];
						 O2 -> O2
					 end,

	Server = couchbeam:server_connection(Url, Args),
	{ok, Database} = couchbeam:open_or_create_db(Server, DB, DBargs),

	{ok, #state{server = Server, database = Database} }.

stop() ->
	gen_server:cast(?MODULE, stop).

handle_call({new_id, N}, _From, S) ->
	UU = couchbeam:get_uuids(S#state.server, N),
	{reply, UU, S};

handle_call({save, D}, _From, S) ->
	{ok, {D2} } = couchbeam:save_doc(S#state.database, {D}),
	{reply, D2, S};

handle_call({retrieve, Id, Args}, _From, S) ->
	Resp = case couchbeam:open_doc(S#state.database, Id, Args) of
					 {ok, {D} } -> D;
					 {error, not_found} -> undefined;
					 {error, R} -> {error, R}
				 end,
	{reply, Resp, S}.

handle_cast(_Message, S) -> {noreply, S}.

handle_info(_Message, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVersion, S, _Extra) -> {ok, S}.
