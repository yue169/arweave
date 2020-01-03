-module(ar_poa).
-export([validate_data_root/2, validate_data_tree/2, validate_chunk/3]).
-include("ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% This module implements all mechanisms required to validate a proof of access
%%% for a chunk of data received from the network.

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