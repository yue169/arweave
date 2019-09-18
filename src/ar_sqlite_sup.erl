-module(ar_sqlite_sup).
-behaviour(supervisor).

%% Public interface

-export([start_link/0]).

%% Supervisor callback

-export([init/1]).

start_link() ->
	supervisor:start_link(?MODULE, []).

init([]) ->
	SupFlags = #{
		strategy => one_for_one,
		intensity => 1,
		period => 5
	},
	ChildSpec = #{
		id => ar_sqlite,
		start => {ar_sqlite, start_link, []}
	},
	{ok, {SupFlags, [ChildSpec]}}.
