-module(ar_test_node).

-export([start/1, start/2, start/3, slave_start/1, slave_start/2]).
-export([connect_to_slave/0, disconnect_from_slave/0]).
-export([slave_call/3, slave_call/4]).
-export([gossip/2, slave_gossip/2, slave_add_tx/2, slave_mine/1]).
-export([wait_until_height/2, slave_wait_until_height/2]).
-export([assert_slave_wait_until_height/2]).
-export([wait_until_block_block_index/2]).
-export([assert_wait_until_block_block_index/2]).
-export([wait_until_receives_txs/2]).
-export([assert_wait_until_receives_txs/2]).
-export([assert_slave_wait_until_receives_txs/2]).
-export([post_tx_to_slave/2, post_tx_to_master/2, post_tx_to_master/3]).
-export([assert_post_tx_to_slave/2, assert_post_tx_to_master/2]).
-export([sign_tx/1, sign_tx/2, sign_tx/3]).
-export([sign_v1_tx/1, sign_v1_tx/2, sign_v1_tx/3]).
-export([get_tx_anchor/0, get_tx_anchor/1]).
-export([join/1, join_on_slave/0, join_on_master/0]).
-export([get_last_tx/1, get_last_tx/2]).
-export([get_tx_confirmations/2]).
-export([get_balance/1]).
-export([test_with_mocked_functions/2]).
-export([get_tx_price/1]).
-export([post_and_mine/2]).
-export([start_without_clear/2]).

-include("src/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

start(no_block) ->
	[B0] = ar_weave:init([]),
	start(B0, unclaimed);
start(B0) ->
	start(B0, unclaimed).

start(B0, RewardAddr) ->
	ar_storage:write_full_block(B0),
	start(B0, {127, 0, 0, 1, slave_call(ar_meta_db, get, [port])}, RewardAddr).

slave_start(no_block) ->
	[B0] = slave_call(ar_weave, init, [[]]),
	slave_start(B0, unclaimed);
slave_start(B0) ->
	slave_start(B0, unclaimed).

slave_start(B0, RewardAddr) ->
	slave_call(ar_storage, write_full_block, [B0]),
	slave_call(?MODULE, start, [B0, {127, 0, 0, 1, ar_meta_db:get(port)}, RewardAddr]).

stop_node() ->
	ar_tx_queue:stop(),
	ar_downloader:reset(),
	ok = kill_if_running([http_bridge_node, http_entrypoint_node]),
	ok = gen_server:stop(ar_data_sync).

start(B0, Peer, RewardAddr) ->
	ar_storage:clear(),
	stop_node(),
	ar_data_sync:reset(),
	Node = ar_node:start([], [B0], 0, RewardAddr),
	ar_http_iface_server:reregister(http_entrypoint_node, Node),
	ar_meta_db:reset_peer(Peer),
	Bridge = ar_bridge:start([], Node, ar_meta_db:get(port)),
	ar_http_iface_server:reregister(http_bridge_node, Bridge),
	ar_node:add_peers(Node, Bridge),
	{Node, B0}.

start_without_clear(B0, RewardAddr) ->
	ar_storage:write_full_block(B0),
	ar_tx_queue:stop(),
	ar_downloader:reset(),
	Peer = {127, 0, 0, 1, slave_call(ar_meta_db, get, [port])},
	Node = ar_node:start([], [B0], 0, RewardAddr),
	ar_http_iface_server:reregister(http_entrypoint_node, Node),
	ar_meta_db:reset_peer(Peer),
	Bridge = ar_bridge:start([], Node, ar_meta_db:get(port)),
	ar_http_iface_server:reregister(http_bridge_node, Bridge),
	ar_node:add_peers(Node, Bridge),
	{Node, B0}.

kill_if_running([Name | Names]) ->
	case whereis(Name) of
		undefined ->
			do_nothing;
		PID ->
			exit(PID, kill)
	end,
	kill_if_running(Names);
kill_if_running([]) ->
	ok.

join_on_slave() ->
	join({127, 0, 0, 1, slave_call(ar_meta_db, get, [port])}).

join(Peer) ->
	stop_node(),
	Node = ar_node:start([Peer]),
	ar_http_iface_server:reregister(http_entrypoint_node, Node),
	ar_meta_db:reset_peer(Peer),
	Bridge = ar_bridge:start([], Node, ar_meta_db:get(port)),
	ar_http_iface_server:reregister(http_bridge_node, Bridge),
	ar_node:add_peers(Node, Bridge),
	Node.

join_on_master() ->
	slave_call(ar_test_node, join, [{127, 0, 0, 1, ar_meta_db:get(port)}]).

connect_to_slave() ->
	%% Connect the nodes by making two HTTP calls.
	%%
	%% After a request to a peer, the peer is recorded in ar_meta_db but
	%% not in the remote peer list. So we need to remove it from ar_meta_db
	%% otherwise it's not added to the remote peer list when it makes a request
	%% to us in turn.
	MasterPort = ar_meta_db:get(port),
	slave_call(ar_meta_db, reset_peer, [{127, 0, 0, 1, MasterPort}]),
	SlavePort = slave_call(ar_meta_db, get, [port]),
	{ok, {{<<"200">>, <<"OK">>}, _, _, _, _}} =
		ar_http:req(#{
			method => get,
			peer => {127, 0, 0, 1, SlavePort},
			path => "/info",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(MasterPort)}]
		}),
	ar_meta_db:reset_peer({127, 0, 0, 1, SlavePort}),
	{ok, {{<<"200">>, <<"OK">>}, _, _, _, _}} =
		ar_http:req(#{
			method => get,
			peer => {127, 0, 0, 1, MasterPort},
			path => "/info",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(SlavePort)}]
		}).

disconnect_from_slave() ->
	%% Disconnects master from slave so that they do not share blocks
	%% and transactions unless they were bound by ar_node:add_peers/2.
	%% All HTTP requests made in this module are made with the
	%% x-p2p-port HTTP header corresponding to the listening port of
	%% the receiving node so that nodes do not start peering with each
	%% other again without an explicit request.
	SlaveBridge = slave_call(erlang, whereis, [http_bridge_node]),
	slave_call(ar_bridge, set_remote_peers, [SlaveBridge, []]),
	MasterBridge = whereis(http_bridge_node),
	ar_bridge:set_remote_peers(MasterBridge, []).

gossip(off, Node) ->
	ar_node:set_loss_probability(Node, 1);
gossip(on, Node) ->
	ar_node:set_loss_probability(Node, 0).

slave_call(Module, Function, Args) ->
	slave_call(Module, Function, Args, 60000).

slave_call(Module, Function, Args, Timeout) ->
	ar_rpc:call(slave, Module, Function, Args, Timeout).

slave_gossip(off, Node) ->
	slave_call(?MODULE, gossip, [off, Node]);
slave_gossip(on, Node) ->
	slave_call(?MODULE, gossip, [on, Node]).

slave_add_tx(Node, TX) ->
	slave_call(ar_node, add_tx, [Node, TX]).

slave_mine(Node) ->
	slave_call(ar_node, mine, [Node]).

wait_until_height(Node, TargetHeight) ->
	{ok, BI} = ar_util:do_until(
		fun() ->
			case ar_node:get_blocks(Node) of
				BI when length(BI) - 1 == TargetHeight ->
					{ok, BI};
				_ ->
					false
			end
		end,
		100,
		60 * 1000
	),
	BI.

slave_wait_until_height(Node, TargetHeight) ->
	slave_call(?MODULE, wait_until_height, [Node, TargetHeight]).

assert_slave_wait_until_height(Node, TargetHeight) ->
	BI = slave_call(?MODULE, wait_until_height, [Node, TargetHeight]),
	?assert(is_list(BI)),
	BI.

assert_wait_until_block_block_index(Node, BI) ->
	?assertEqual(ok, wait_until_block_block_index(Node, BI)).

wait_until_block_block_index(Node, BI) ->
	ar_util:do_until(
		fun() ->
			case ar_node:get_blocks(Node) of
				BI ->
					ok;
				_ ->
					false
			end
		end,
		100,
		60 * 1000
	).

assert_wait_until_receives_txs(Node, TXs) ->
	?assertEqual(ok, wait_until_receives_txs(Node, TXs)).

wait_until_receives_txs(Node, TXs) ->
	ar_util:do_until(
		fun() ->
			MinedTXIDs = [TX#tx.id || TX <- ar_node:get_mined_txs(Node)],
			case lists:all(fun(TX) -> lists:member(TX#tx.id, MinedTXIDs) end, TXs) of
				true ->
					ok;
				_ ->
					false
			end
		end,
		100,
		10 * 1000
	).

assert_slave_wait_until_receives_txs(Node, TXs) ->
	?assertEqual(ok, slave_call(?MODULE, wait_until_receives_txs, [Node, TXs])).

post_tx_to_slave(Slave, TX) ->
	SlavePort = slave_call(ar_meta_db, get, [port]),
	SlaveIP = {127, 0, 0, 1, SlavePort},
	Reply =
		ar_http:req(#{
			method => post,
			peer => SlaveIP,
			path => "/tx",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(SlavePort)}],
			body => ar_serialize:jsonify(ar_serialize:tx_to_json_struct(TX))
		}),
	case Reply of
		{ok, {{<<"200">>, _}, _, <<"OK">>, _, _}} ->
			assert_slave_wait_until_receives_txs(Slave, [TX]);
		_ ->
			ar:console(
				"Failed to post transaction. Error DB entries: ~p~n",
				[slave_call(ar_tx_db, get_error_codes, [TX#tx.id])]
			),
			noop
	end,
	Reply.

assert_post_tx_to_slave(Slave, TX) ->
	{ok, {{<<"200">>, _}, _, <<"OK">>, _, _}} = post_tx_to_slave(Slave, TX).

assert_post_tx_to_master(Master, TX) ->
	{ok, {{<<"200">>, _}, _, <<"OK">>, _, _}} = post_tx_to_master(Master, TX).

post_tx_to_master(Master, TX) ->
	post_tx_to_master(Master, TX, true).

post_tx_to_master(Master, TX, Wait) ->
	Port = ar_meta_db:get(port),
	MasterIP = {127, 0, 0, 1, Port},
	Reply =
		ar_http:req(#{
			method => post,
			peer => MasterIP,
			path => "/tx",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}],
			body => ar_serialize:jsonify(ar_serialize:tx_to_json_struct(TX))
		}),
	case Reply of
		{ok, {{<<"200">>, _}, _, <<"OK">>, _, _}} ->
			case Wait of
				true ->
					assert_wait_until_receives_txs(Master, [TX]);
				false ->
					ok
			end;
		_ ->
			ar:console(
				"Failed to post transaction. Error DB entries: ~p~n",
				[ar_tx_db:get_error_codes(TX#tx.id)]
			),
			noop
	end,
	Reply.

sign_tx(Wallet) ->
	sign_tx(slave, Wallet, #{ format => 2 }, fun ar_tx:sign/2).

sign_tx(Wallet, TXParams) ->
	sign_tx(slave, Wallet, insert_root(TXParams#{ format => 2 }), fun ar_tx:sign/2).

sign_tx(Node, Wallet, TXParams) ->
	sign_tx(Node, Wallet, insert_root(TXParams#{ format => 2 }), fun ar_tx:sign/2).

insert_root(Params) ->
	case {maps:get(data, Params, <<>>), maps:get(data_root, Params, <<>>)} of
		{<<>>, _} ->
			Params;
		{Data, <<>>} ->
			TX = ar_tx:generate_chunk_tree(#tx{ data = Data }),
			Params#{ data_root => TX#tx.data_root };
		_ ->
			Params
	end.

sign_v1_tx(Wallet) ->
	sign_tx(slave, Wallet, #{}, fun ar_tx:sign_v1/2).

sign_v1_tx(Wallet, TXParams) ->
	sign_tx(slave, Wallet, TXParams, fun ar_tx:sign_v1/2).

sign_v1_tx(Node, Wallet, TXParams) ->
	sign_tx(Node, Wallet, TXParams, fun ar_tx:sign_v1/2).

sign_tx(Node, Wallet, TXParams, SignFun) ->
	{_, Pub} = Wallet,
	Data = maps:get(data, TXParams, <<>>),
	DataSize = maps:get(data_size, TXParams, byte_size(Data)),
	Reward = case maps:get(reward, TXParams, none) of
		none ->
			get_tx_price(Node, DataSize);
		AssignedReward ->
			AssignedReward
	end,
	SignFun(
		(ar_tx:new())#tx {
			owner = Pub,
			reward = Reward,
			data = Data,
			target = maps:get(target, TXParams, <<>>),
			quantity = maps:get(quantity, TXParams, 0),
			tags = maps:get(tags, TXParams, []),
			last_tx = maps:get(last_tx, TXParams, <<>>),
			data_size = DataSize,
			data_root = maps:get(data_root, TXParams, <<>>),
			format = maps:get(format, TXParams, 1)
		},
		Wallet
	).

get_tx_anchor() ->
	get_tx_anchor(slave).

get_tx_anchor(slave) ->
	SlavePort = slave_call(ar_meta_db, get, [port]),
	IP = {127, 0, 0, 1, SlavePort},
	{ok, {{<<"200">>, _}, _, Reply, _, _}} =
		ar_http:req(#{
			method => get,
			peer => IP,
			path => "/tx_anchor",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(SlavePort)}]
		}),
	ar_util:decode(Reply);
get_tx_anchor(master) ->
	Port = ar_meta_db:get(port),
	IP = {127, 0, 0, 1, Port},
	{ok, {{<<"200">>, _}, _, Reply, _, _}} =
		ar_http:req(#{
			method => get,
			peer => IP,
			path => "/tx_anchor",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
		}),
	ar_util:decode(Reply).

get_last_tx(Key) ->
	get_last_tx(slave, Key).

get_last_tx(slave, {_, Pub}) ->
	Port = slave_call(ar_meta_db, get, [port]),
	IP = {127, 0, 0, 1, Port},
	{ok, {{<<"200">>, _}, _, Reply, _, _}} =
		ar_http:req(#{
			method => get,
			peer => IP,
			path => "/wallet/" ++ binary_to_list(ar_util:encode(ar_wallet:to_address(Pub))) ++ "/last_tx",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
		}),
	ar_util:decode(Reply);
get_last_tx(master, {_, Pub}) ->
	Port = ar_meta_db:get(port),
	IP = {127, 0, 0, 1, Port},
	{ok, {{<<"200">>, _}, _, Reply, _, _}} =
		ar_http:req(#{
			method => get,
			peer => IP,
			path => "/wallet/" ++ binary_to_list(ar_util:encode(ar_wallet:to_address(Pub))) ++ "/last_tx",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
		}),
	ar_util:decode(Reply).

get_tx_confirmations(slave, TXID) ->
	Port = slave_call(ar_meta_db, get, [port]),
	IP = {127, 0, 0, 1, Port},
	Response =
		ar_http:req(#{
			method => get,
			peer => IP,
			path => "/tx/" ++ binary_to_list(ar_util:encode(TXID)) ++ "/status",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
		}),
	case Response of
		{ok, {{<<"200">>, _}, _, Reply, _, _}} ->
			{Status} = ar_serialize:dejsonify(Reply),
			lists:keyfind(<<"number_of_confirmations">>, 1, Status);
		{ok, {{<<"404">>, _}, _, _, _, _}} ->
			-1
	end;
get_tx_confirmations(master, TXID) ->
	Port = ar_meta_db:get(port),
	IP = {127, 0, 0, 1, Port},
	Response =
		ar_http:req(#{
			method => get,
			peer => IP,
			path => "/tx/" ++ binary_to_list(ar_util:encode(TXID)) ++ "/status",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
		}),
	case Response of
		{ok, {{<<"200">>, _}, _, Reply, _, _}} ->
			{Status} = ar_serialize:dejsonify(Reply),
			lists:keyfind(<<"number_of_confirmations">>, 1, Status);
		{ok, {{<<"404">>, _}, _, _, _, _}} ->
			-1
	end.

get_balance(Pub) ->
	Port = slave_call(ar_meta_db, get, [port]),
	IP = {127, 0, 0, 1, Port},
	{ok, {{<<"200">>, _}, _, Reply, _, _}} =
		ar_http:req(#{
			method => get,
			peer => IP,
			path => "/wallet/" ++ binary_to_list(ar_util:encode(ar_wallet:to_address(Pub))) ++ "/balance",
			headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
		}),
	binary_to_integer(Reply).

test_with_mocked_functions(Functions, TestFun) ->
	{
		foreach,
		fun() ->
			lists:foldl(
				fun({Module, Fun, Mock}, Mocked) ->
					NewMocked = case maps:get(Module, Mocked, false) of
						false ->
							meck:new(Module, [passthrough]),
							slave_call(meck, new, [Module, [no_link, passthrough]]),
							maps:put(Module, true, Mocked);
						true ->
							Mocked
					end,
					meck:expect(Module, Fun, Mock),
					slave_call(meck, expect, [Module, Fun, Mock]),
					NewMocked
				end,
				maps:new(),
				Functions
			)
		end,
		fun(Mocked) ->
			maps:fold(
				fun(Module, _, _) ->
					meck:unload(Module),
					slave_call(meck, unload, [Module])
				end,
				noop,
				Mocked
			)
		end,
		[
			{timeout, 120, TestFun}
		]
	}.

get_tx_price(DataSize) ->
	get_tx_price(slave, DataSize).

get_tx_price(Node, DataSize) ->
	{IP, Port} = case Node of
		slave ->
			P = slave_call(ar_meta_db, get, [port]),
			{{127, 0, 0, 1, P}, P};
		master ->
			P = ar_meta_db:get(port),
			{{127, 0, 0, 1, P}, P}
	end,
	{ok, {{<<"200">>, _}, _, Reply, _, _}} = case Node of
		slave ->
			ar_http:req(#{
				method => get,
				peer => IP,
				path => "/price/" ++ integer_to_binary(DataSize),
				headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
			});
		master ->
			ar_http:req(#{
				method => get,
				peer => IP,
				path => "/price/" ++ integer_to_binary(DataSize),
				headers => [{<<"X-P2p-Port">>, integer_to_binary(Port)}]
			})
	end,
	binary_to_integer(Reply).

post_and_mine(#{ miner := Miner, await_on := AwaitOn }, TXs) ->
	CurrentHeight = case Miner of
		{slave, MiningNode} ->
			Height = slave_call(ar_node, get_height, [MiningNode]),
			lists:foreach(fun(TX) -> assert_post_tx_to_slave(MiningNode, TX) end, TXs),
			slave_mine(MiningNode),
			Height;
		{master, MiningNode} ->
			Height = ar_node:get_height(MiningNode),
			lists:foreach(fun(TX) -> assert_post_tx_to_master(MiningNode, TX) end, TXs),
			ar_node:mine(MiningNode),
			Height
	end,
	case AwaitOn of
		{master, AwaitNode} ->
			wait_until_height(AwaitNode, CurrentHeight + 1),
			H = ar_node:get_current_block_hash(AwaitNode),
			BShadow = ar_storage:read_block(H),
			BShadow#block{ txs = ar_storage:read_tx(BShadow#block.txs) };
		{slave, AwaitNode} ->
			slave_wait_until_height(AwaitNode, CurrentHeight + 1),
			H = slave_call(ar_node, get_current_block_hash, [AwaitNode]),
			BShadow = slave_call(ar_storage, read_block, [H]),
			BShadow#block{ txs = slave_call(ar_storage, read_tx, [BShadow#block.txs]) }
	end.
