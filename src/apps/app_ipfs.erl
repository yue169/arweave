-module(app_ipfs).
-export([
	start/2, start/3,
	start_link/1, start_pinning/0,
	stop/1,
	get_and_send/2,
	bulk_get_and_send/2, bulk_get_and_send_from_file/2,
	get_txs/1,
	get_local_ipfs_txs/0, add_local_ipfs_tx_data/0, add_local_ipfs_tx_data/1,
	ipfs_hash_status/1,
	maybe_ipfs_add_txs/1,
	report/1]).
-export([confirmed_transaction/2]). % for adt_simple
-export([add_ipfs_data/2, add_ipfs_data/3, pinned/3]). % spawned from server
-include("../ar.hrl").

-ifdef(DEBUG).
-define(MAX_TX_BUFFER, 5).
-else.
-define(MAX_TX_BUFFER, 50).
-endif.

-record(state,{
	adt_pid,
	wallet,
	queue,
	ipfs_name,
	ipfs_key,
	txs = []
}).

%%% api

%% @doc Start pinning incoming tagged TXs to a local IPFS node.
start_pinning() ->
	Node = whereis(http_entrypoint_node),
	Wallet = undefined,
	Name = "",
	{ok, _Pid} = start([Node], Wallet, Name),
	ok.

start(Peers, Wallet) ->
	start(Peers, Wallet, "").

start(Peers, Wallet, Name) ->
	ar_ipfs:daemon_start(),
	Queue = case Wallet of
		undefined -> undefined;
		_         -> app_queue:start(Wallet)
	end,
	PidMod = case Name of
		"" ->
			spawn(fun() -> server(#state{
				queue=Queue, wallet=Wallet
			}) end);
		_  ->
			{ok, Key} = make_identity(Name),
			spawn(fun() -> server(#state{
				queue=Queue, wallet=Wallet, ipfs_name=Name, ipfs_key=Key
			}) end)
		end,
	register(?MODULE, PidMod),
	PidADT = adt_simple:start(?MODULE, PidMod),
	lists:foreach(fun(Node) -> ar_node:add_peers(Node, [PidADT]) end, Peers),
	PidMod ! {add_adt_pid, PidADT},
	{ok, PidMod}.

%% @doc Start a node, linking to a supervisor process
%% function and doc comment copied from other {ar,app}_*:start_link functions.
start_link(Args) ->
	PID = erlang:apply(app_ipfs, start, Args),
	{ok, PID}.

stop(Pid) ->
	Pid ! stop,
	ar_ipfs:daemon_stop().

get_and_send(Pid, Hashes) ->
	Pins = ar_ipfs:pin_ls(),
	HashesToGet = lists:filter(fun(H) -> not(lists:member(H, Pins)) end,
		Hashes),
	Q = get_x(Pid, get_queue, queue),
	lists:foreach(fun(Hash) ->
			spawn(fun() -> get_hash_and_queue(Hash, Q) end)
		end,
		HashesToGet).

bulk_get_and_send(Pid, Hashes) ->
	Pins = ar_ipfs:pin_ls(),
	HashesToGet = lists:filter(fun(H) -> not(lists:member(H, Pins)) end,
		Hashes),
	Q = get_x(Pid, get_queue, queue),
	lists:foreach(fun(Hash) -> get_hash_and_queue(Hash, Q) end, HashesToGet).

bulk_get_and_send_from_file(Pid, Filename) ->
	{ok, Hashes} = file:consult(Filename),
	bulk_get_and_send(Pid, Hashes).

get_txs(Pid) ->
	get_x(Pid, get_txs, txs).

get_local_ipfs_txs() ->
	ar_tx_search:get_entries_by_tag_name(<<"IPFS-Add">>).

add_local_ipfs_tx_data() ->
	TXids = get_local_ipfs_txs(),
	lists:foreach(fun add_local_ipfs_tx_data/1, TXids).

add_local_ipfs_tx_data(TXid) ->
	case ar_storage:read_tx(TXid) of
		unavailable -> {error, tx_unavailable};
		TX ->
			case lists:keyfind(<<"IPFS-Add">>, 1, TX#tx.tags) of
				{<<"IPFS-Add">>, Hash} ->
					add_ipfs_data(TX, Hash);
				false ->
					{error, hash_not_found}
			end
	end.

ipfs_hash_status(Hash) ->
	Pinned = is_pinned(Hash),
	TXIDs = ar_tx_search:get_entries(<<"IPFS-Add">>, Hash),
	[{hash, Hash}, {pinned, Pinned}, {tx, TXIDs}].

maybe_ipfs_add_txs(TXs) ->
	case whereis(?MODULE) of
		undefined -> not_running;
		Pid ->
			case is_process_alive(Pid) of
				false -> not_running;
				true ->
					lists:foreach(
						fun(TX) -> confirmed_transaction(Pid, TX) end,
						TXs)
			end
	end.

pinned(undefined, _, _) ->
	ok;
pinned(Pid, TXidEnc, IPFSHash) ->
	Pid ! {pinned, TXidEnc, IPFSHash}.
	

report(Pid) ->
	get_x(Pid, get_report, report).

%%% adt_simple callbacks
%%% return the new state (i.e. always the server pid)

confirmed_transaction(Pid, TX) ->
	Pid ! {recv_new_tx, TX},
	Pid.

%%% local server

server(State=#state{
			adt_pid=ADTPid, queue=Q, wallet=Wallet,
			ipfs_name=Name, ipfs_key=Key,
			txs=TXs}) ->
	receive
		stop ->
			State#state.adt_pid ! stop,
			ok;
		{add_adt_pid, Pid} ->
			server(State#state{adt_pid=Pid});
		{get_report, From} ->
			Report = [
				with_aliveness(ADTPid, adt_pid),
				with_aliveness(Q, queue),
				{wallet, Wallet},
				{ipfs_name, Name}, {ipfs_key, Key},
				{txs, length(TXs), safe_hd(TXs)}],
			From ! {report, Report},
			server(State);
		{get_txs, From} ->
			From ! {txs, TXs},
			server(State);
		{get_queue, From} ->
			From ! {queue, Q},
			server(State);
		{pinned, TXidEnc, IPFSHash} ->
			ar:report({app_ipfs, pinned, TXidEnc, IPFSHash}),
			server(State#state{txs=maybe_append_tx(TXs, {TXidEnc, IPFSHash})});
		{queue_tx, UnsignedTX} ->
			app_queue:add(Q, UnsignedTX),
			server(State);
		{recv_new_tx, TX=#tx{tags=Tags, id=ID}} ->
			ar:report({app_ipfs, recv_new_tx, ar_util:encode(ID)}),
			case lists:keyfind(<<"IPFS-Add">>, 1, Tags) of
				{<<"IPFS-Add">>, Hash} ->
					spawn(?MODULE, add_ipfs_data, [TX, Hash, self()]);
				false ->
					pass
			end,
			server(State);
		{recv_new_tx, X} ->
			ar:report({app_ipfs, recv_new_tx, X}),
			server(State)
	end.

%%% private functions

add_ipfs_data(TX, Hash) ->
	add_ipfs_data(TX, Hash, undefined).
add_ipfs_data(TX, Hash, From) ->
	TXide = ar_util:encode(TX#tx.id),
	ar:d({recv_tx_ipfs_add, TXide, Hash}),
	case ar_ipfs:add_data(TX#tx.data, Hash) of
		{ok, Response} ->
			?MODULE:pinned(From, TXide, Hash),
			spawn(ar_ipfs, dht_provide_hash, [Hash]),
			{ok, Response};
		{error, Reason} ->
			{error, Reason}
	end.

maybe_append_tx(TXs, TX) ->
	case lists:member(TX, TXs) of
		true  -> TXs;
		false -> lists_nthhead(?MAX_TX_BUFFER, [TX|TXs])
	end.

lists_nthhead(N, Xs) when length(Xs) > N ->
	{X,_} = lists:split(N, Xs),
	X;
lists_nthhead(_, Xs) ->
	Xs.

get_hash_and_queue(Hash, Queue) ->
	ar:d({fetching, Hash}),
	case ar_ipfs:cat_data_by_hash(Hash) of
		{ok, Data} ->
			{ok, Hash2} = ar_ipfs:add_data(Data, Hash),
			ar:d({added, Hash, Hash2}),
			UnsignedTX = #tx{tags=[{<<"IPFS-Add">>, Hash}], data=Data},
			app_queue:add(Queue, UnsignedTX),
			ok;
		{error, Reason} ->
			{error, Reason}
	end.

get_x(Pid, SendTag, RecvTag) ->
	Pid ! {SendTag, self()},
	receive
		{RecvTag, X} -> X
	end.

is_pinned(Hash) ->
	Pins = ar_ipfs:pin_ls(),
	lists:member(Hash, Pins).

make_identity(Name) ->
	{ok, Key} = ar_ipfs:key_gen(Name),
	ok = ar_ipfs:config_set_identity(Key),
	{ok, Key}.

safe_hd([])    -> [];
safe_hd([H|_]) -> H.

with_aliveness(undefined, Label) ->
	{Label, undefined, false};
with_aliveness(Pid, Label) when is_atom(Pid) ->
	with_aliveness(whereis(Pid), Label);
with_aliveness(Pid, Label) ->
	{Label, Pid, is_process_alive(Pid)}.
