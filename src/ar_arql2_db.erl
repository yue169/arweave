-module(ar_arql2_db).

-include("ar.hrl").

%% API
-export([import/0, eval/1, sample_query/1]).

import() ->
	BHL = ar_node:get_hash_list(whereis(http_entrypoint_node)),
	timer:tc(fun() ->
		lists:foreach(fun(BH) -> insert_block(BH, BHL) end, BHL)
	end).

insert_block(BH, BHL) ->
	case ar_storage:read_block(BH, BHL) of
		B when is_record(B, block) ->
			BlockIndepHash = ar_util:encode(B#block.indep_hash),
			{ok, _} = ar_sqlite:exec("
				INSERT INTO block
				(indep_hash, previous_block, height, timestamp)
				VALUES (?, ?, ?, ?)
			", [
				BlockIndepHash,
				ar_util:encode(B#block.previous_block),
				B#block.height,
				B#block.timestamp
			]),
			lists:foreach(fun(TXID) -> insert_tx(TXID, BlockIndepHash) end, B#block.txs);
		unavailable ->
			ok
	end.

insert_tx(TXID, BlockIndepHash) ->
	case ar_storage:read_tx(TXID) of
		TX when is_record(TX, tx) ->
			TXHash = ar_util:encode(TX#tx.id),
			{ok, _} = ar_sqlite:exec("
				INSERT INTO tx
				(id, block_indep_hash, last_tx, owner, from_address, target, quantity, signature, reward)
				VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)
			", [
				TXHash,
				BlockIndepHash,
				ar_util:encode(TX#tx.last_tx),
				ar_util:encode(TX#tx.owner),
				ar_util:encode(ar_wallet:to_address(TX#tx.owner)),
				ar_util:encode(TX#tx.target),
				TX#tx.quantity,
				ar_util:encode(TX#tx.signature),
				TX#tx.reward
			]),
			lists:foreach(fun(Tag) -> insert_tag(Tag, TXHash) end, TX#tx.tags);
		unavailable ->
			ok
	end.

insert_tag({Name, Value}, TXHash) ->
	{ok, _} = ar_sqlite:exec("
		INSERT INTO tag
		(tx_id, name, value)
		VALUES (?, ?, ?)
	", [TXHash, Name, Value]).

sample_query(1) ->
	{'and',{equals,<<"App-Name">>,<<"arweave-id">>},
		{'and',{equals,<<"from">>,
			<<"vLRHFqCw1uHu75xqB4fCDW-QxpkpJxBtFD9g4QYUbfw">>},
			{equals,<<"Type">>,<<"name">>}}};
sample_query(2) ->
	{'and',
		{equals,<<"from">>,<<"xC2lHGxVhFTekOlM5IobtgyjmfQI5g7hIm5yHnQyhTw">>},
		{equals,<<"User-Agent">>, <<"ArweaveChrome/2.1.4">>}}.

eval(Query) ->
	{WhereClause, Params} = eval_where(Query),
	{ok, Results} = ar_sqlite:q("
		SELECT tx.id
		FROM tx
		JOIN tag ON tag.tx_id = tx.id
		WHERE " ++ WhereClause ++ "
		ORDER BY tx.id ASC
	", Params),
	lists:map(fun({TXID}) -> TXID end, Results).

eval_where({equals, <<"from">>, Value}) ->
	{"tx.from_address = ?", [Value]};
eval_where({equals, Key, Value}) ->
	{"(tag.name = ? AND tag.value = ?)", [Key, Value]};
eval_where({'and',E1,E2}) ->
	{S1, P1} = eval_where(E1),
	{S2, P2} = eval_where(E2),
	{"(" ++ S1 ++ " AND " ++ S2 ++ ")", P1 ++ P2};
eval_where({'or',E1,E2}) ->
	{S1, P1} = eval(E1),
	{S2, P2} = eval(E2),
	{"(" ++ S1 ++ " OR " ++ S2 ++ ")", P1 ++ P2}.
