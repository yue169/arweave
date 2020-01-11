-module(ar_fork_recovery).
-export([start/6]).
-include("ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% An asynchronous process that asks another node on a different fork
%%% for all of the blocks required to 'catch up' with their state of the
%%% network - each block is verified in turn.
%%% Once all the blocks since the two forks of the netwrok diverged have been
%%% verified, the process returns this new state to its parent.
%%%
%%% Block shadows transmit the last 50 hashes of the block index, as such if a
%%% node falls more than this limit behind the fork recovery process will fail.

%% Defines the server state
-record(state, {
	parent, % Fork recoveries parent process (initiator)
	peers, % Lists of the nodes peers to try retrieve blocks
	target_block, % The target block being recovered too
	recovery_block_index, % Complete hash list for this fork
	done, % List of hashes of verified blocks
	to_go, % List of block hashes needing to be verified and applied (lowest to highest)
	block_txs_pairs % List of {BH, TXIDs} pairs of the last ?MAX_TX_ANCHOR_DEPTH blocks
}).

%% @doc Start the fork recovery 'catch up' server.
%% TargetBShadow - a block shadow with the reconstructed hash list.
start(Peers, TrustedPeers, TargetBShadow, BI, Parent, BlockTXPairs) ->
	case ?IS_BLOCK(TargetBShadow) of
		true ->
			ar:info(
				[
					{started_fork_recovery_proc, Parent},
					{block, ar_util:encode(TargetBShadow#block.indep_hash)},
					{target_height, TargetBShadow#block.height},
					{top_peers, lists:sublist(Peers, 5)},
					{top_trusted_peers, lists:sublist(TrustedPeers, 5)}
				]
			),
			% Ensures that the block is within the recovery range and is has
			% been validly rebuilt from a block shadow.
			case
				TargetBShadow#block.height == length(TargetBShadow#block.block_index)
			of
				true ->
					PID =
						spawn(
							fun() ->
								TargetB = TargetBShadow,
								TargetIndex = {TargetB#block.indep_hash, TargetB#block.weave_size},
								DivergedHashes = drop_until_diverge(
									lists:reverse(TargetB#block.block_index),
									lists:reverse(BI)
								) ++ [TargetIndex],
								Done = (TargetB#block.block_index -- DivergedHashes),
								server(
									#state {
										parent = Parent,
										peers = Peers,
										done = Done,
										to_go = DivergedHashes,
										target_block = TargetB,
										recovery_block_index =
											[TargetIndex|TargetB#block.block_index],
										block_txs_pairs = get_block_txs_pairs(BlockTXPairs, Done, TrustedPeers)
									}
								)
							end
						),
					PID ! apply_next_block,
					PID;
				% target block has invalid hash list
				false ->
					ar:warn(
						[
							could_not_start_fork_recovery,
							{reason, target_block_block_index_incorrect}
						]
				),
				undefined
			end;
		false ->
			ar:warn(
				[
					could_not_start_fork_recovery,
					{reason, could_not_retrieve_target_block}
				]
			),
			undefined
	end.

%% @doc Take two lists, drop elements until they do not match.
%% Return the remainder of the _first_ list.
drop_until_diverge([X | R1], [X | R2]) -> drop_until_diverge(R1, R2);
drop_until_diverge(R1, _) -> R1.

get_block_txs_pairs(BlockTXPairs, BI, TrustedPeers) ->
	CutBlockTXPairs = cut_block_txs_pairs(BlockTXPairs, element(1, hd(BI))),
	case {length(BI), length(CutBlockTXPairs)} of
		{L, _} when L =< ?MAX_TX_ANCHOR_DEPTH ->
			CutBlockTXPairs;
		{_, ?MAX_TX_ANCHOR_DEPTH} ->
			CutBlockTXPairs;
		{_, L} when L < ?MAX_TX_ANCHOR_DEPTH ->
			CutBlockTXPairs ++
			lists:map(
				fun(Depth) ->
					{BH, _} = lists:nth(Depth, BI),
					B = read_or_fetch_block(BH, BI, TrustedPeers),
					{BH, B#block.txs}
				end,
				lists:seq(L + 1, ?MAX_TX_ANCHOR_DEPTH)
			)
	end.

cut_block_txs_pairs(BlockTXPairs = [{BH, _} | _], BH) ->
	BlockTXPairs;
cut_block_txs_pairs([], _) ->
	[];
cut_block_txs_pairs([_ | Rest], BH) ->
	cut_block_txs_pairs(Rest, BH).

read_or_fetch_block(BH, BI, TrustedPeers) ->
	case ar_storage:read_block(BH, BI) of
		unavailable ->
			ar_node_utils:get_full_block(TrustedPeers, BH, BI);
		B ->
			B
	end.

%% @doc Subtract the second list from the first. If the first list
%% is not a superset of the second, return the empty list
setminus([X | R1], [X | R2]) -> setminus(R1, R2);
setminus(R1, []) -> R1;
setminus(_, _) -> [].

%% @doc Start the fork recovery server loop. Attempt to catch up to the
%% target block by applying each block between the current block and the
%% target block in turn.
server(#state {
		peers = _Peers,
		parent = _Parent,
		target_block = _TargetB
	}, rejoin) -> ok.
server(#state {
		done = Done,
		block_txs_pairs = BlockTXPairs,
		to_go = [],
		parent = Parent
	}) ->
	Parent ! {fork_recovered, Done, BlockTXPairs};
server(S = #state { target_block = TargetB }) ->
	receive
		{parent_accepted_block, B} ->
			if B#block.height > TargetB#block.height ->
				ar:info(
					[
						stopping_fork_recovery,
						{reason, parent_accepted_higher_block_than_target}
					]
				),
				ok;
			true ->
				ar:info(
					[
						continuing_fork_recovery,
						{reason, parent_accepted_lower_block_than_target}
					]
				),
				server(S)
			end
	after 0 ->
		do_fork_recover(S)
	end.

do_fork_recover(S = #state {
		done = Done,
		peers = Peers,
		to_go = [{NextH, _} | BI],
		target_block = TargetB,
		recovery_block_index = RBI,
		parent = Parent,
		block_txs_pairs = BlockTXPairs
	}) ->
	receive
	{update_target_block, Block, Peer} ->
		NewBI =
			[
				{
					Block#block.indep_hash,
					Block#block.weave_size
				}
			| Block#block.block_index],
		% If the new retarget blocks block index contains the hash of the last
		% retarget should be recovering to the same fork.
		NewToVerify =
			case lists:member({TargetB#block.indep_hash, TargetB#block.weave_size}, NewBI) of
				true ->
					ar:info([encountered_block_on_same_fork_as_recovery_process]),
					drop_until_diverge(
						lists:reverse(NewBI),
						lists:reverse(Done)
					);
				false ->
					ar:info([encountered_block_on_different_fork_to_recovery_process]),
					[]
			end,
		case NewToVerify =/= [] of
			true ->
				ar:info(
					[
						updating_fork_recovery_target,
						{current_target_height, TargetB#block.height},
						{current_target_hash, ar_util:encode(TargetB#block.indep_hash)},
						{new_target_height, Block#block.height},
						{new_target_hash, ar_util:encode(Block#block.indep_hash)},
						{still_to_verify, length(NewToVerify)}
					]
				),
				NewPeers =
					ar_util:unique(
						Peers ++
						if is_list(Peer) -> Peer;
						true -> [Peer]
						end
					),
				server(
					S#state {
						to_go = NewToVerify,
						peers = NewPeers,
						target_block = Block,
						recovery_block_index = NewBI
					}
				);
			false ->
				ar:info(
					[
						not_updating_target_block,
						{ignored_block, ar_util:encode(Block#block.indep_hash)},
						{height, Block#block.height}
					]
				),
				server(S)
		end;
	apply_next_block ->
		NextB = ar_node_utils:get_full_block(Peers, NextH, RBI),
		ar:info(
			[
				{applying_fork_recovery, ar_util:encode(NextH)}
			]
		),
		case ?IS_BLOCK(NextB) of
			% could not retrieve the next block to be applied
			false ->
				ar:warn(
					[
						{fork_recovery_block_retreival_failed, ar_util:encode(NextH)},
						{received_instead, NextB}
					]
				),
				BBI = unavailable,
				B = unavailable,
				POA = unavailable,
				TXs = [];
			% next block retrieved, applying block
			true ->
				% Ensure that block being applied is not the genesis block and
				% is within the range of fork recovery.
				%%
				% TODO: Duplication of check, target block height is checked
				% when fork recovery process starts.
				case
					{
						NextB#block.height,
						((TargetB#block.height - NextB#block.height) >
							?STORE_BLOCKS_BEHIND_CURRENT)
					}
				of
					% Recovering to genesis block
					{0, _} ->
						ar:err(
							[
								fork_recovery_failed,
								recovery_block_is_genesis_block
							]
						),
						BBI = unavailable,
						B = unavailable,
						POA = unavailable,
						TXs = [],
						server(S, rejoin);
					% Target block is too far ahead and cannot be recovered.
					{_, true} ->
						ar:err(
							[
								fork_recovery_failed,
								recovery_block_is_too_far_ahead
							]
						),
						BBI = unavailable,
						B = unavailable,
						POA = unavailable,
						TXs = [],
						server(S, rejoin);
					% Target block is within range and is attempted to be
					% recovered to.
					{_X, _Y} ->
						B = ar_node:get_block(Peers, NextB#block.previous_block, RBI),
						case ?IS_BLOCK(B) of
							false ->
								BBI = unavailable,
								POA = unavailable,
								TXs = [];
							true ->
								BBI = [{B#block.indep_hash, B#block.weave_size}|B#block.block_index],
								POA =
									case NextB#block.poa of
										undefined -> ar_poa:generate(NextB);
										X -> X
									end,
								%% TODO: Rewrite validate so it also takes recall block txs
								TXs = NextB#block.txs
						end
				end
		end,
		% Ensure next block (NextB) is a block, the previous block (B) is a
		% block and that the nexts blocks recall block (RecallB) is a block.
		case
			(not ?IS_BLOCK(NextB)) or
			(not ?IS_BLOCK(B)) or
			(not is_record(POA, poa))
		of
			false ->
				case
					try_apply_block(
						BBI,
						NextB#block {
							txs = [T#tx.id || T <- NextB#block.txs]
						},
						TXs,
						B,
						POA,
						BlockTXPairs
					)
				of
					{error, invalid_block} ->
						ar:report_console(
							[
								could_not_validate_fork_block,
								{next_block, ?IS_BLOCK(NextB)},
								{block, ?IS_BLOCK(B)},
								{poa, POA}
							]
						);
					{error, tx_replay} ->
						ar:err([
							ar_fork_recovery,
							tx_replay_detected,
							{
								block_indep_hash,
								ar_util:encode(NextB#block.indep_hash)
							},
							{
								txs,
								lists:map(
									fun(TX) -> ar_util:encode(TX#tx.id) end,
									TXs
								)
							},
							{
								block_txs_pairs,
								lists:map(
									fun({BH, TXIDs}) ->
										{ar_util:encode(BH), lists:map(fun ar_util:encode/1, TXIDs)}
									end,
									BlockTXPairs
								)
							}
						]);
					ok ->
						ar:info(
							[
								{applied_fork_recovery_block, ar_util:encode(NextH)},
								{block_height, NextB#block.height}
							]
						),
						NewBlockTXPairs = ar_node_utils:update_block_txs_pairs(
							NextB#block.indep_hash,
							[TX#tx.id || TX <- NextB#block.txs],
							BlockTXPairs
						),
						case ar_meta_db:get(partial_fork_recovery) of
							true ->
								ar:info(
									[
										reported_partial_fork_recovery,
										{height, NextB#block.height}
									]
								),
								Parent ! {fork_recovered, [ {NextH, NextB#block.weave_size} | Done], NewBlockTXPairs};
							_ -> do_nothing
						end,
						self() ! apply_next_block,
						ar_storage:write_full_block(NextB),
						server(
							S#state {
								done = [ {NextH, NextB#block.weave_size} | Done],
								block_txs_pairs = NewBlockTXPairs,
								to_go = BI
							}
						)
				end;
			true -> server(S#state { to_go = [] } )
		end;
	_ -> server(S)
	end.

%% @doc Try and apply a new block (NextB) to the current block (B).
%% Returns	true if the block can be applied, otherwise false.
try_apply_block(BI, NextB, TXs, B, POA, BlockTXPairs) ->
	{FinderReward, _} =
		ar_node_utils:calculate_reward_pool(
			B#block.reward_pool,
			TXs,
			NextB#block.reward_addr,
			POA,
			NextB#block.weave_size,
			NextB#block.height,
			NextB#block.diff,
			NextB#block.timestamp
		),
	WalletList =
		ar_node_utils:apply_mining_reward(
			ar_node_utils:apply_txs(B#block.wallet_list, TXs, B#block.height),
			NextB#block.reward_addr,
			FinderReward,
			NextB#block.height
		),
	BlockValid = ar_node_utils:validate(
		BI,
		WalletList,
		NextB#block { poa = POA },
		TXs,
		B,
		POA,
		NextB#block.reward_addr,
		NextB#block.tags
	),
	case BlockValid of
		{invalid, _} ->
			{error, invalid_block};
		valid ->
			TXReplayCheck = ar_tx_replay_pool:verify_block_txs(
				TXs,
				NextB#block.diff,
				B#block.height,
				NextB#block.timestamp,
				B#block.wallet_list,
				BlockTXPairs
			),
			case TXReplayCheck of
				invalid ->
					{error, tx_replay};
				valid ->
					ok
			end
	end.

%%%
%%% Tests: ar_fork_recovery
%%%

%% @doc Ensure forks that are one block behind will resolve.
three_block_ahead_recovery_test() ->
	ar_storage:clear(),
	Node1 = ar_node:start(),
	Node2 = ar_node:start(),
	B0 = ar_weave:init([]),
	ar_storage:write_block(hd(B0)),
	B1 = ar_weave:add(B0, []),
	ar_storage:write_block(hd(B1)),
	B2 = ar_weave:add(B1, []),
	ar_storage:write_block(hd(B2)),
	B3 = ar_weave:add(B2, []),
	ar_storage:write_block(hd(B3)),
	Node1 ! Node2 ! {replace_block_list, B3},
	timer:sleep(500),
	ar_node:mine(Node1),
	timer:sleep(500),
	ar_node:mine(Node2),
	timer:sleep(500),
	ar_node:mine(Node1),
	timer:sleep(500),
	ar_node:mine(Node1),
	timer:sleep(500),
	ar_node:add_peers(Node1, Node2),
	timer:sleep(500),
	ar_node:mine(Node1),
	timer:sleep(1000),
	?assertEqual(block_hashes_by_node(Node1), block_hashes_by_node(Node2)),
	?assertEqual(8, length(block_hashes_by_node(Node2))).

block_hashes_by_node(Node) ->
	BHs = ar_node:get_blocks(Node),
	Bs = [ar_storage:read_block(BH, ar_node:get_block_index(Node)) || BH <- BHs],
	[ar_util:encode(B#block.indep_hash) || B <- Bs].

%% @doc Ensure that nodes on a fork that is far behind will catchup correctly.
multiple_blocks_ahead_recovery_test() ->
	ar_storage:clear(),
	Node1 = ar_node:start(),
	Node2 = ar_node:start(),
	B0 = ar_weave:init([]),
	ar_storage:write_block(hd(B0)),
	B1 = ar_weave:add(B0, []),
	ar_storage:write_block(hd(B1)),
	B2 = ar_weave:add(B1, []),
	ar_storage:write_block(hd(B2)),
	B3 = ar_weave:add(B2, []),
	ar_storage:write_block(hd(B3)),
	Node1 ! Node2 ! {replace_block_list, B3},
	ar_node:mine(Node1),
	ar_node:mine(Node2),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:add_peers(Node1, Node2),
	ar_node:mine(Node1),
	timer:sleep(1500),
	?assertEqual(block_hashes_by_node(Node1), block_hashes_by_node(Node2)),
	?assertEqual(10, length(block_hashes_by_node(Node2))).

%% @doc Ensure that nodes on a fork that is far behind blocks that contain
%% transactions will catchup correctly.
multiple_blocks_ahead_with_transaction_recovery_test_() ->
	{timeout, 60, fun() ->
		ar_storage:clear(),
		{Priv1, Pub1} = ar_wallet:new(),
		{_Priv2, Pub2} = ar_wallet:new(),
		TX = ar_tx:new(Pub2, ?AR(1), ?AR(9000), <<>>),
		SignedTX = ar_tx:sign(TX, Priv1, Pub1),
		Node1 = ar_node:start(),
		Node2 = ar_node:start(),
		B0 = ar_weave:init([]),
		ar_storage:write_block(hd(B0)),
		B1 = ar_weave:add(B0, []),
		ar_storage:write_block(hd(B1)),
		B2 = ar_weave:add(B1, []),
		ar_storage:write_block(hd(B2)),
		B3 = ar_weave:add(B2, []),
		ar_storage:write_block(hd(B3)),
		Node1 ! Node2 ! {replace_block_list, B3},
		ar_node:mine(Node1),
		ar_node:mine(Node2),
		receive after 300 -> ok end,
		ar_node:add_tx(Node1, SignedTX),
		ar_node:mine(Node1),
		receive after 300 -> ok end,
		ar_node:mine(Node1),
		receive after 300 -> ok end,
		ar_node:mine(Node1),
		receive after 300 -> ok end,
		ar_node:mine(Node1),
		receive after 300 -> ok end,
		ar_node:add_peers(Node1, Node2),
		ar_node:mine(Node1),
		receive after 1500 -> ok end,
		?assertEqual(block_hashes_by_node(Node1), block_hashes_by_node(Node2)),
		?assertEqual(10, length(block_hashes_by_node(Node2)))
	end}.

%% @doc Ensure that nodes that have diverged by multiple blocks each can
%% reconcile.
multiple_blocks_since_fork_test() ->
	ar_storage:clear(),
	Node1 = ar_node:start(),
	Node2 = ar_node:start(),
	B0 = ar_weave:init([]),
	ar_storage:write_block(hd(B0)),
	B1 = ar_weave:add(B0, []),
	ar_storage:write_block(hd(B1)),
	B2 = ar_weave:add(B1, []),
	ar_storage:write_block(hd(B2)),
	B3 = ar_weave:add(B2, []),
	ar_storage:write_block(hd(B3)),
	Node1 ! Node2 ! {replace_block_list, B3},
	ar_node:mine(Node1),
	ar_node:mine(Node2),
	timer:sleep(300),
	ar_node:mine(Node1),
	ar_node:mine(Node2),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:add_peers(Node1, Node2),
	ar_node:mine(Node1),
	timer:sleep(1500),
	?assertEqual(block_hashes_by_node(Node1), block_hashes_by_node(Node2)),
	?assertEqual(10, length(block_hashes_by_node(Node2))).

%% @doc Ensure that nodes that nodes recovering from the first block can
%% reconcile.
% fork_from_first_test() ->
%	ar_storage:clear(),
%	B1 = ar_weave:init([]),
%	Node1 = ar_node:start([], B1),
%	Node2 = ar_node:start(Node1, B1),
%	ar_node:mine(Node1),
%	receive after 300 -> ok end,
%	ar_node:mine(Node1),
%	receive after 300 -> ok end,
%	ar_node:add_peers(Node1, Node2),
%	ar_node:mine(Node1),
%	receive after 300 -> ok end,
%	true = (ar_node:get_blocks(Node1) == ar_node:get_blocks(Node2)).

%% @doc Check the logic of setminus will correctly update to a new fork
setminus_test() ->
	ar_storage:clear(),
	Node1 = ar_node:start(),
	Node2 = ar_node:start(),
	B0 = ar_weave:init([]),
	ar_storage:write_block(hd(B0)),
	B1 = ar_weave:add(B0, []),
	ar_storage:write_block(hd(B1)),
	B2 = ar_weave:add(B1, []),
	ar_storage:write_block(hd(B2)),
	B3 = ar_weave:add(B2, []),
	ar_storage:write_block(hd(B3)),
	Node1 ! Node2 ! {replace_block_list, B3},
	ar_node:mine(Node1),
	timer:sleep(300),
	ar_node:mine(Node1),
	timer:sleep(300),
	LengthLong = length(
		setminus(lists:reverse(ar_node:get_blocks(Node1)),
		lists:reverse(ar_node:get_blocks(Node2)))
	),
	ar_node:mine(Node1),
	ar_node:mine(Node2),
	timer:sleep(300),
	LengthShort = length(
		setminus(
			lists:reverse(ar_node:get_blocks(Node1)),
			lists:reverse(ar_node:get_blocks(Node2)))
		),
	LengthLong = 2,
	LengthShort = 0.
