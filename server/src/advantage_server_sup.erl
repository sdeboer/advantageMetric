-module(advantage_server_sup).

-behaviour(supervisor).
-export([start_link/0]).
-export([init/1]).

-define(CHILD(I),
				{I, {I, start_link, []}, permanent, 5000, worker, [I]}).
-define(SUP(I),
				{I, {I, start_link, []}, permanent, infinity, supervisor, [I]}).

start_link() ->
	supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
	Children = [
							?CHILD(riot),
							?CHILD(objectives)
						 ],
	{ok, { {one_for_one, 5, 10}, Children} }.
