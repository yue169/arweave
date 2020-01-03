-module(ar_poa).
-export([validate_data_root/2, validate_data_tree/2, validate_chunk/3]).
-export([validate_data_tree_lengths/2]).
-include("ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% This module implements all mechanisms required to validate a proof of access
%%% for a chunk of data received from the network.

%% @doc Validate that an untrusted chunk index (probably received from another peer)
%% matches the chunk index hash of a transaction.
validate_data_root(TX, ChunkIndex) ->
	TX2 = ar_tx:generate_data_root(TX#tx { data_tree = ChunkIndex }),
	(TX#tx.data_root == TX2#tx.data_root) andalso
		validate_data_tree_lengths(TX#tx.chunk_hash_size, ChunkIndex).

%% @doc Validate that the chunk index against the entire TX data.
validate_data_tree(TX, Data) ->
	TX2 = ar_tx:generate_data_tree(TX#tx { data = Data }),
	TX#tx.data_tree == TX2#tx.data_tree.

%% @doc Validate a single chunk from a chunk index matches.
validate_chunk(TX, ChunkNum, Chunk) ->
	ChunkID = lists:nth(ChunkNum, TX#tx.data_tree),
	ChunkID == ar_tx:generate_chunk_id(TX, Chunk).

validate_data_tree_lengths(HashSize, ChunkIndex) ->
	lists:all(fun(Hash) -> byte_size(Hash) == HashSize end, ChunkIndex).

validate_chunking_test() ->
	% Generate our TX data, wallet, and signed and indexed transactions.
	TXData = crypto:strong_rand_bytes(trunc(?DATA_CHUNK_SIZE * 5.5)),
	{Priv, Pub} = ar_wallet:new(),
	UnsignedTX =
		ar_tx:generate_data_root(
			ar_tx:generate_data_tree(
				#tx {
					format = 2,
					data = TXData,
					data_size = byte_size(TXData),
					reward = ?AR(100)
				}
			)
		),
	SignedTX = ar_tx:sign(UnsignedTX#tx { data = <<>> }, Priv, Pub),
	% Extract the data and indexes needed for proof verification.
	% In practice these will be received over the wire from another node,
	% or from disk.
    RecvdTX = SignedTX#tx { data_tree = [] },
	ChunkIndex = UnsignedTX#tx.data_tree,
	ChallengeChunk = 3,
    Chunk =
		binary:part(
			TXData,
			trunc(?DATA_CHUNK_SIZE*(ChallengeChunk-1)),
			?DATA_CHUNK_SIZE
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
    ?assert(validate_data_root(RecvdTX, ChunkIndex)),
    RecvdTX2 = RecvdTX#tx { data_tree = ChunkIndex },
    ?assert(validate_chunk(RecvdTX2, 3, Chunk)).