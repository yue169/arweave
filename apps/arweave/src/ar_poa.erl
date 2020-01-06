-module(ar_poa).
-export([validate/4]).
-export([validate_data_root/2, validate_data_tree/2, validate_chunk/3]).
-include("ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% This module implements all mechanisms required to validate a proof of access
%%% for a chunk of data received from the network.

%% @doc Validate a complete proof of access object.
validate(LastIndepHash, WeaveSize, BHL, POA) ->
	ChallengeByte = calculate_challenge_byte(LastIndepHash, WeaveSize, POA#poa.option),
	ChallengeBlock = calculate_challenge_block(ChallengeByte, BHL),
	case is_old_poa(ChallengeBlock, BHL) of
		true -> validate_old_poa(BHL, POA);
		false -> validate_recall_block(ChallengeByte, ChallengeBlock, POA)
	end.

calculate_challenge_byte(LastIndepHash, WeaveSize, Option) ->
	binary:decode_unsigned(multihash(LastIndepHash, Option)) rem WeaveSize.

multihash(X, Remaining) when Remaining =< 0 -> X;
multihash(X, Remaining) ->
	multihash(crypto:hash(?HASH_ALG, X), Remaining - 1).

calculate_challenge_block(ChallengeByte, [{BH, WeaveSize}|_])
		when WeaveSize > ChallengeByte -> BH;
calculate_challenge_block(ChallengeByte, [_|BHL]) ->
	calculate_challenge_block(ChallengeByte, BHL).

calculate_block_height(BH, [{BH, _}|_BHL]) -> 0;
calculate_block_height(BH, [_|BHL]) ->
	calculate_block_height(BH, BHL).

is_old_poa(ChallengeBlock, BHL) ->
	calculate_block_height(ChallengeBlock, BHL) =< ar_fork:height_2_0().

validate_old_poa(_, _) ->
	error(not_implemented).

validate_recall_block(ChallengeByte, ChallengeBH, POA) ->
	case ar_weave:indep_hash(POA#poa.recall_block) of
		ChallengeBH -> validate_tx_path(ChallengeByte, POA);
		_ -> false
	end.

validate_tx_path(ChallengeByte, POA) ->
	BlockOffset = ChallengeByte - (POA#poa.recall_block)#block.weave_size,
	Validation =
		ar_merkle:validate_path(
			(POA#poa.recall_block)#block.tx_root,
			BlockOffset,
			POA#poa.tx_path
		),
	case Validation of
		false -> false;
		TXID -> validate_tx(TXID, BlockOffset, POA)
	end.

validate_tx(TXID, BlockOffset, POA) when TXID == (POA#poa.tx)#tx.id ->
	case ar_tx:verify_after_mining(POA#poa.tx) of
		true ->
			validate_data_path(BlockOffset, POA);
		false -> false
	end;
validate_tx(_, _, _) -> false.

validate_data_path(BlockOffset, POA) ->
	% Calculate TX offsets within the block
	TXEndOffset = ar_merkle:extract_note(POA#poa.tx_path),
	TXStartOffset = TXEndOffset - (POA#poa.tx)#tx.data_size,
	TXOffset = BlockOffset - TXStartOffset,
	Validation =
		ar_merkle:validate_path(
			(POA#poa.tx)#tx.data_root,
			TXOffset,
			POA#poa.data_path
		),
	case Validation of
		false -> false;
		ChunkID ->
			validate_chunk(ChunkID, POA)
	end.

validate_chunk(ChunkID, POA) ->
	ChunkID == ar_tx:generate_chunk_id(POA#poa.chunk).

%% @doc Validate that an untrusted chunk index (probably received from another peer)
%% matches the chunk index hash of a transaction.
validate_data_root(TX, ChunkIndex) ->
	TX2 = ar_tx:generate_data_root(TX#tx { data_tree = ChunkIndex }),
	(TX#tx.data_root == TX2#tx.data_root).

%% @doc Validate that the chunk index against the entire TX data.
validate_data_tree(TX, Data) ->
	TX2 = ar_tx:generate_data_tree(TX#tx { data = Data }),
	TX#tx.data_tree == TX2#tx.data_tree.

%% @doc Validate a single chunk from a chunk index matches.
validate_chunk(TX, ChunkNum, Chunk) ->
	ChunkID = lists:nth(ChunkNum, TX#tx.data_tree),
	ChunkID == ar_tx:generate_chunk_id(TX, Chunk).

validate_chunking_test() ->
	% Generate our TX data, wallet, and signed and indexed transactions.
	TXData = crypto:strong_rand_bytes(trunc(?DATA_CHUNK_SIZE * 5.5)),
	ChallengeLocation = (?DATA_CHUNK_SIZE * 3) + 10, % Pick a byte in the third chunk
		Chunk =
		binary:part(
			TXData,
			trunc(?DATA_CHUNK_SIZE*3),
			?DATA_CHUNK_SIZE
		),
	{Priv, Pub} = ar_wallet:new(),
	UnsignedTX =
		ar_tx:generate_data_tree(
			#tx {
				format = 2,
				data = TXData,
				data_size = byte_size(TXData),
				reward = ?AR(100)
			}
		),
	SignedTX = ar_tx:sign(UnsignedTX#tx { data = <<>>, data_tree = [] }, Priv, Pub),
	DataPath =
		ar_merkle:generate_path(
			SignedTX#tx.data_root,
			ChallengeLocation,
			UnsignedTX#tx.data_tree
		),
	% Verify each level of the index and the chunk itself.
	Diff = 1,
	Height = 0,
	Timestamp = os:system_time(seconds),
	?assert(
		ar_tx:verify(
			SignedTX,
			Diff,
			Height,
			[{ar_wallet:to_address(Pub), ?AR(100), <<>>}],
			Timestamp
		)
	),
	RealChunkID = ar_tx:generate_chunk_id(Chunk),
	PathChunkID = ar_merkle:validate_path(SignedTX#tx.data_root, ChallengeLocation, DataPath),
	?assertEqual(RealChunkID, PathChunkID),
	% In this case, we know the chunk is valid is it is generated in the test, but in the real
	% world, this chunk would come from the proof of access received from the network.
	?assertEqual(PathChunkID, ar_tx:generate_chunk_id(Chunk)).