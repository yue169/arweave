-module(ar_api).
-export([receive_new_block/6]).
-include("ar.hrl").

receive_new_block(BShadow, RecallSize, OrigPeer, RecallHash, Key, Nonce) ->
	spawn(
		fun() ->
			CurrentB = ar_node:get_current_block(whereis(http_entrypoint_node)),
			B = ar_block:generate_block_from_shadow(BShadow, RecallSize),
			case (not is_atom(CurrentB)) andalso
				(B#block.height > CurrentB#block.height) andalso
				(B#block.height =< (CurrentB#block.height + 50)) andalso
				(B#block.diff >= ?MIN_DIFF) of
				true ->
					ar:report([{
						sending_external_block_to_bridge,
						ar_util:encode(BShadow#block.indep_hash)
					}]),
					RecallB =
						ar_block:get_recall_block(
							OrigPeer,
							RecallHash,
							B,
							Key,
							Nonce,
							CurrentB#block.hash_list
						),
					ar_bridge:add_block(
						whereis(http_bridge_node),
						OrigPeer, B, RecallB, Key, Nonce
					);
				_ ->
					ok
			end
		end
	).

