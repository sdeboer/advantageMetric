-module(persist).
-behaviour(gen_server).

-export([start_link/0]).

% gen_server API
-export([init/1, handle_call/3, handle_cast/2,
	terminate/2, handle_info/2, 
	code_change/3, stop/1]).

% Model API
-export([
	save/2, 
	load/1,
	dumb_load/1
	]).

% Key API
-export([
	generate/0, gen/0, del/1, exists/1
	]).

% List API
-export([
	list/1,
	delist/1,
	push/2, pop/1, pop/2,
	unshift/2, shift/1, shift/2,
	items/1 %, hasitem/2, length/1,
	%range/3,
	]).

% Set API
-export([
	set/1, deset/1,
	add/2, remove/2,
	contains/2, members/1,
	cardinality/1, card/1,
	intersection/1, interstore/2,
	union/1, unionstore/2
	]).

% Hash API
-export([
	hash_get/2, hash_set/3, hash_del/2,
	hash_all/1
	]).

start_link() ->
	gen_server:start_link({global, ?MODULE}, ?MODULE, [], []).

mod_name()->
	% Why does ?MODULE not just work?  Why global?
	global:whereis_name(?MODULE).

% Keys
generate() -> list_to_binary(uuid:to_string(uuid:uuid4())).

gen() -> generate().

del(K) ->
	command(["DEL", K]).

exists(K)->
	command(["EXISTS", K]).

% Models
save(Key, Value) ->
	gen_server:call(mod_name(),
		{save_value, Key, Value}
		).

dumb_load(Key) ->
	gen_server:call(mod_name(), {get_value, Key}).

load(Key) ->
	case gen_server:call(mod_name(), {get_value, Key}) of
		undefined -> undefined;
		Data -> binary_to_term(Data)
	end.

% Lists
-record(list, { key, persisted = false, dirty = true }).

list(K) -> #list{key = K}.

delist(S) when is_record(S, list) ->
	del(S#list.key),
	{ok, S#list{persisted = false, dirty = true}};

delist(K) -> 
	del(K),
	{ok, #list{key = K}}.

push([], S) -> S;

push(List, S) ->
	command(["RPUSH", S#list.key | List]),
	S#list{ persisted = true }.

unshift([], L) -> L;

unshift(List, S) ->
	command(["LPUSH", S#list.key | List]),
	S#list{ persisted = true }.

pop(L) ->
	% check undefined (empty list)
	V = command(["LPOP", L#list.key]),
	{V, L}.

pop(N, L)->
	Vs = command(["LRANGE", L#list.key, 0, N - 1]),
	Len = length(Vs),
	if Len > 0 -> 
			command(["LREM", L#list.key, Len]);
		true -> nop
	end,
	{Vs, L}.

shift(S) ->
	% check undefined (empty list)
	V = command(["RPOP", S#list.key]),
	{V, S}.

shift(N, L)->
	Vs = command(["RRANGE", L#list.key, 0, N -1]),
	Len = length(Vs),
	if Len > 0 -> 
			command(["RREM", L#list.key, Len]);
		true -> nop
	end,
	{Vs, L}.

items(S) ->
	V = command(["LRANGE", S#list.key, 0, -1]),
	{V, S#list{persisted = true, dirty = false} }.

% Sets

-record(set, { key, persisted = false, dirty = true }).

set(K) -> #set{key = K}.

deset(S) when is_record(S, set) ->
	del(S#set.key),
	{ok, S#set{persisted = false, dirty = true}};

deset(K) ->
	del(K),
	{ok, #set{key = K}}.

add(List, S) ->
	command(["SADD", S#set.key | List]),
	{ok, S#set{ persisted = true }}.

remove(List, S) ->
	command(["SREM", S#set.key | List]),
	{ok, S}.

contains(Item, S) ->
	R = case command(["SISMEMBER", S#set.key, Item]) of
		<<"1">> -> true;
		<<"0">> -> false
	end,
	{R, S}.

members(S) ->
	{command(["SMEMBERS", S#set.key]), S}.

cardinality(S) ->
	{command(["SCARD", S#set.key]), S}.

card(S) -> cardinality(S).

keylist(SList) ->
	lists:map(fun(S) -> S#set.key end, SList).

intersection(SList) ->
	command(["SINTER" | keylist(SList)]).

interstore(Dest, SList) when is_record(Dest, set) ->
	command(["SINTERSTORE", Dest#set.key, keylist(SList)]),
	Dest#set{persisted = true};

interstore(DK, SList) ->
	interstore(#set{key = DK}, SList).

union(SList) ->
	command(["SUNION" | keylist(SList)]).

unionstore(Dest, SList) when is_record(Dest, set) ->
	command(["SUNIONSTORE", Dest#set.key | keylist(SList)]),
	Dest#set{persisted = true};

unionstore(DK, SList) ->
	unionstore(#set{key = DK}, SList).

% Hash API
hash_get(K, F) ->
	command(["HGET", K, F]).

hash_set(K, F, V) ->
	command(["HSET", K, F, V]).

hash_all(K) ->
	to_proplist(command(["HGETALL", K])).

hash_del(K, F) ->
	command(["HDEL", K, F]).

% private

init([]) ->
	{ok, Redis} = eredis:start_link(),
	{ok, Redis}.

stop(_Pid) ->
	stop().

stop() ->
	gen_server:cast(mod_name(), stop).

command(Arg) ->
	gen_server:call(mod_name(), {command, Arg}).

handle_call({get_value, Key}, _From, Redis) ->
	{ok, Value} = eredis:q(Redis, ["GET", Key]),
	{reply, Value, Redis};

handle_call({save_value, Key, Value}, _From, Redis) ->
	%lager:debug("Saving ~p / ~p", [Key, Value]),
	{ok, <<"OK">>} = eredis:q(Redis, ["SET", Key, Value]),
	{reply, ok, Redis};

handle_call({command, List}, _From, Redis) ->
	%lager:debug("command ~p", [List]),
	{ok, Value} = eredis:q(Redis, List),
	{reply, Value, Redis};

handle_call(_Message, _From, Redis) ->
	{reply, error, Redis}.

handle_cast(_Message, Redis) -> {noreply, Redis}.
handle_info(_Message, Redis) -> {noreply, Redis}.
terminate(_Reason, _Redis) -> ok.
code_change(_OldVersion, Redis, _Extra) -> {ok, Redis}.

to_proplist([]) -> [];
to_proplist([K, V | T]) -> [{K, V} | to_proplist(T)].
