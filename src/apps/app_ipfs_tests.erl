-module(app_ipfs_tests).
-include("../ar.hrl").
-include_lib("eunit/include/eunit.hrl").

runs_while_daemon_down_test_() ->
	{timeout, 60, fun() ->
		{Node, Wallet, IPFSPid} = setup(),
		true = ar_ipfs:daemon_is_running(),
		TXid1 = send_ipfs_tx_mine_blocks(Node, Wallet, <<>>),
		TXid1e = ar_util:encode(TXid1),
		wait_till_msg_q_empty(),
		[_|_] = app_ipfs:report(app_ipfs),
		ar:report({app_ipfs_tests, report, app_ipfs:report(app_ipfs)}),
		[{TXid1e, Label}] = app_ipfs:get_txs(IPFSPid),
		ar:report({app_ipfs_tests, ipfs_daemon_stop}),
		{ok, _Response} = ar_ipfs:daemon_stop(),
		timer:sleep(1000),
		false = ar_ipfs:daemon_is_running(),
		TXid2 = send_ipfs_tx_mine_blocks(Node, Wallet, TXid1),
		[{TXid1e, Label}] = app_ipfs:get_txs(IPFSPid),
		ok = ar_ipfs:daemon_start(),
		ar:report({app_ipfs_tests, ipfs_daemon_start}),
		true = ar_ipfs:daemon_is_running(),
		TXid3 = send_ipfs_tx_mine_blocks(Node, Wallet, TXid2),
		TXid3e = ar_util:encode(TXid3),
		[{TXid3e,_},{TXid1e, Label}] = app_ipfs:get_txs(IPFSPid),
		closedown(IPFSPid)
	end}.

% not_sending_already_got_test_() ->
% 	{timeout, 60, fun() ->
% 		{_, IPFSPid} = setup(),
% 		{HashTups, _} = app_ipfs_utils:ep_get_ipfs_hashes(3, 123),
% 		Hashes = ar_ipfs:hashes_only(HashTups),
% 		ar:d({here, Hashes}),
% 		app_ipfs:get_and_send(app_ipfs, Hashes),
% 		timer:sleep(3000),
% 		app_ipfs:get_and_send(app_ipfs, Hashes),
% 		closedown(IPFSPid)
% 	end}.

%%% private

setup() ->
	{Node, Wallet} = ar_node_init(),
	prepare_tx_adder(Node),
	ok = ar_ipfs:daemon_start(),
	{ok, Pid} = app_ipfs:start([Node], undefined, ""),
	{Node, Wallet, Pid}.

ar_node_init() ->
	ar_storage:clear(),
	W = ar_wallet:new(),
	B = ar_weave:init([{ar_wallet:to_address(W), ?AR(1000), <<>>}]),
	Pid = ar_node:start([], B),
	{Pid, W}.

prepare_tx_adder(Node) ->
	ar_http_iface_server:reregister(Node),
	Bridge = ar_bridge:start([], [], Node),
	ar_http_iface_server:reregister(http_bridge_node, Bridge),
	ar_node:add_peers(Node, Bridge).

send_ipfs_tx_mine_blocks(Node, Wallet, LastTX) ->
	TS = ar_ipfs:rfc3339_timestamp(),
	Filename = <<"testdata.txt">>,
	Data = timestamp_data(TS, <<"Data">>),
	Tags = [{<<"IPFS-Add">>, Filename}], % should be IPFSHash
	Reward = ?AR(1),
	TX = tag_tx(ar_tx:new(Data, Reward, LastTX), Tags),
	STX = ar_tx:sign(TX, Wallet),
	send_tx_mine_blocks(Node, STX),
	STX#tx.id.

send_tx_mine_blocks(Node, TX) ->
	ar_node:add_tx(Node, TX),
	timer:sleep(2000),
	ar_node:mine(Node),
	timer:sleep(2000),
	ok.

closedown(IPFSPid) ->
	app_ipfs:stop(IPFSPid).

tag_tx(TX, Tags) ->
	TX#tx{tags=Tags}.

timestamp_data(TS, Data) ->
	<<TS/binary, "  *  ", Data/binary>>.

wait_till_msg_q_empty() ->
	PI = process_info(whereis(app_ipfs)),
	{_, MQL} = lists:keyfind(message_queue_len, 1, PI),
	ar:report({app_ipfs, message_queue_len, MQL}),
	timer:sleep(1000),
	case MQL =:= 0 of
		true  -> ok;
		false -> wait_till_msg_q_empty()
	end.

% mine_n_blocks_on_node(N, Node) ->
% 	lists:foreach(fun(_) ->
% 			ar_node:mine(Node),
% 			timer:sleep(1000)
% 		end, lists:seq(1,N)),
% 	timer:sleep(1000),
% 	ar_node:get_blocks(Node).

% add_n_txs_to_node(N, Node) ->
% 	% cribbed from ar_http_iface:add_external_tx_with_tags_test/0.
% 	prepare_tx_adder(Node),
% 	lists:map(fun(_) ->
% 			Tags = [
% 				{<<"TEST_TAG1">>, <<"TEST_VAL1">>},
% 				{<<"TEST_TAG2">>, <<"TEST_VAL2">>}
% 			],
% 			TX = tag_tx(ar_tx:new(<<"DATA">>), Tags),
% 			ar_http_iface:send_new_tx({127, 0, 0, 1, 1984}, TX),
% 			receive after 1000 -> ok end,
% 			ar_node:mine(Node),
% 			receive after 1000 -> ok end,
% 			TX#tx.id
% 		end,
% 		lists:seq(1,N)).

% add_n_tx_pairs_to_node(N, Node) ->
% 	prepare_tx_adder(Node),
% 	BoringTags = [
% 		{<<"TEST_TAG1">>, <<"TEST_VAL1">>},
% 		{<<"TEST_TAG2">>, <<"TEST_VAL2">>}
% 	],
% 	lists:map(fun(X) ->
% 			TX1 = tag_tx(ar_tx:new(timestamp_data(<<"DATA">>)), BoringTags),
% 			send_tx_mine_block(Node, TX1),
% 			TS = ar_ipfs:rfc3339_timestamp(),
% 			Filename = numbered_fn(X),
% 			Data = timestamp_data(TS, <<"Data">>),
% 			Tags = [{<<"IPFS-Add">>, Filename}],
% 			TX2 = tag_tx(ar_tx:new(Data), Tags),
% 			send_tx_mine_block(Node, TX2),
% 			TS
% 		end,
% 		lists:seq(1,N)).

% numbered_fn(N) ->
% 	NB = integer_to_binary(N),
% 	<<"testdata-", NB/binary, ".txt">>.

% hashes_only(HashTups) ->
% 	lists:flatten(lists:map(fun
% 		({_,H,<<>>}) -> H;
% 		({_,H,P})    -> [H,P]
% 	end, HashTups)).

% ipfs_hashes_to_data(Pid) ->
% 	lists:map(fun(Hash) ->
% 				{ok, Data} = ar_ipfs:cat_data_by_hash(Hash),
% 				Data
% 		end,
% 		lists:reverse(app_ipfs:get_ipfs_hashes(Pid))).

% ipfs_hashes_to_TSs(Pid) ->
% 	lists:map(fun(Bin) ->
% 				<<TS:25/binary,_/binary>> = Bin,
% 				TS
% 		end,
% 		ipfs_hashes_to_data(Pid)).

