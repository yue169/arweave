-module(ar_sqlite).
-behaviour(gen_server).

-export([start_link/0, q/1, q/2, exec/1, exec/2]).
-export([init/1, handle_call/3, handle_cast/2]).

-define(TRY(Expr), try {ok, Expr} catch {error, E} -> {error, E} end).

-record(s, {conn}).

%% Public interface

start_link() ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

q(Sql) ->
	gen_server:call(?MODULE, {q, Sql, []}).

q(Sql, Args) ->
	gen_server:call(?MODULE, {q, Sql, Args}).

exec(Sql) ->
	gen_server:call(?MODULE, {exec, Sql, []}).

exec(Sql, Args) ->
	gen_server:call(?MODULE, {exec, Sql, Args}).

%% Generic server callbacks

init([]) ->
	PrivDir = code:priv_dir(arweave),
	Filename = filename:join(PrivDir, "arql2.db"),
	{ok, Conn} = esqlite3:open(Filename),
	{ok, #s{ conn = Conn }}.

handle_call({q, Sql, Args}, _, #s { conn = Conn } = State) when is_list(Args) ->
	Reply = ?TRY(esqlite3:q(Sql, Args, Conn)),
	{reply, Reply, State};
handle_call({exec, Sql, Args}, _, #s { conn = Conn } = State) when is_list(Args) ->
	Reply = ?TRY(esqlite3:exec(Sql, Args, Conn)),
	{reply, Reply, State};
handle_call(_, _, State) ->
	{noreply, State}.

handle_cast(_, State) ->
	{noreply, State}.
