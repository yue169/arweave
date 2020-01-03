-module(ar_poa).
-export([validate_chunk_index_hash/2, validate_chunk_index/2, validate_chunk/3]).
-export([validate_chunk_index_lengths/2]).
-include("ar.hrl").
-include_lib("eunit/include/eunit.hrl").

%%% This module implements all mechanisms required to validate a proof of access
%%% for a chunk of data received from the network.

%% @doc Validate that an untrusted chunk index (probably received from another peer)
%% matches the chunk index hash of a transaction.
validate_chunk_index_hash(TX, ChunkIndex) ->
	TX2 = ar_tx:generate_chunk_index_hash(TX#tx { chunk_index = ChunkIndex }),
	(TX#tx.chunk_index_hash == TX2#tx.chunk_index_hash) andalso
		validate_chunk_index_lengths(TX#tx.chunk_hash_size, ChunkIndex).

%% @doc Validate that the chunk index against the entire TX data.
validate_chunk_index(TX, Data) ->
	TX2 = ar_tx:generate_chunk_index(TX#tx { data = Data }),
	TX#tx.chunk_index == TX2#tx.chunk_index.

%% @doc Validate a single chunk from a chunk index matches.
validate_chunk(TX, ChunkNum, Chunk) ->
	ChunkID = lists:nth(ChunkNum, TX#tx.chunk_index),
	ChunkID == ar_tx:generate_chunk_id(TX, Chunk).

validate_chunk_index_lengths(HashSize, ChunkIndex) ->
	lists:all(fun(Hash) -> byte_size(Hash) == HashSize end, ChunkIndex).

validate_chunking_test() ->
	% Generate our TX data, wallet, and signed and indexed transactions.
	TXData = crypto:strong_rand_bytes(trunc(?DATA_CHUNK_SIZE * 5.5)),
	{Priv, Pub} = ar_wallet:new(),
	UnsignedTX =
		ar_tx:generate_chunk_index_hash(
			ar_tx:generate_chunk_index(
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
    RecvdTX = SignedTX#tx { chunk_index = [] },
	ChunkIndex = UnsignedTX#tx.chunk_index,
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
    ?assert(validate_chunk_index_hash(RecvdTX, ChunkIndex)),
    RecvdTX2 = RecvdTX#tx { chunk_index = ChunkIndex },
    ?assert(validate_chunk(RecvdTX2, 3, Chunk)).