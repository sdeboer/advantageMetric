-module(map_reduce).
-behaviour(gen_server).

-export([
				 start_link/0,
				 start_link/1]).

% gen_server API
-export([
				 init/1, terminate/2, stop/0, code_change/3,
				 handle_call/3, handle_cast/2, handle_info/2
				]).

-export([
				 design/1, design/2,
				 make/1, make/2,
				 set_map/3, set_reduce/3
				]).

% Views

-define(DEFAULT_URL, "http://localhost:5984").
-define(DEFAULT_DB, "advantage").

-record(state, {
					server,
					database
				 }).

-define(DEFAULT_LANGUAGE, "erlang").
-define(VIEW_LABEL, <<"views">>).
-define(MAP_LABEL, <<"map">>).
-define(REDUCE_LABEL, <<"reduce">>).

design_id(Design) ->
	B1 = <<"_design/">>,
	B2 = list_to_binary(Design),
	<<B1/binary, B2/binary>>.

make(Design) -> make(Design, ?DEFAULT_LANGUAGE).

make(Design, Language) ->
	[
	 {<<"_id">>, design_id(Design) },
	 {<<"language">>, list_to_binary(Language)},
	 {<<"views">>, {[]} }
	].

design(Name) -> design(Name, ?DEFAULT_LANGUAGE).

design(Name, Language) ->
	case document:get(design_id(Name)) of
		undefined -> make(Name, Language);
		V -> V
	end.

set_map(Name, Function, Design) ->
	set_view_subset(?MAP_LABEL, Name, Function, Design).

set_reduce(Name, Function, Design) ->
	set_view_subset(?REDUCE_LABEL, Name, Function, Design).

set_view_subset(Sub, Name, Function, Design) ->
	Fn = list_to_binary(Function),
	Nm = list_to_binary(Name),
	{Views} = proplists:get_value(?VIEW_LABEL, Design),
	View = case proplists:get_value(Nm, Views) of
					 undefined -> [];
					 {V} -> V
				 end,
	V2 = [ {Sub, Fn} | proplists:delete(Sub, View)],
	Vs2 = [ {Nm, {V2}} | proplists:delete(Nm, Views) ],
	D2 = [ {?VIEW_LABEL, {Vs2} } | proplists:delete(?VIEW_LABEL, Design) ],
	D2.

handle_call({view, Design, View, Opts}, _F, S) ->
	Resp = case couchbeam_view:fetch(S#state.database, {Design, View}, Opts) of
					 {ok, View} -> View;
					 {error, Condition} -> {error, Condition}
				 end,

	{reply, Resp, S}.


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

handle_cast(_Message, S) -> {noreply, S}.

handle_info(_Message, S) -> {noreply, S}.

terminate(_Reason, _S) -> ok.

code_change(_OldVersion, S, _Extra) -> {ok, S}.
