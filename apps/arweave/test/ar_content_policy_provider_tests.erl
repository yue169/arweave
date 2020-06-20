-module(ar_content_policy_provider_tests).

-include("src/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ar_test_node, [
	start/1,
	sign_tx/2,
	wait_until_height/2,
	post_and_mine/2,
	get_tx_anchor/1,
	assert_post_tx_to_master/2
]).

removes_blacklisted_txs_test_() ->
	{timeout, 60, fun removes_blacklisted_txs/0}.

removes_blacklisted_txs() ->
	AR = ?AR(20),
	Key1 = {_, Pub1} = ar_wallet:new(),
	Key2 = {_, Pub2} = ar_wallet:new(),
	Key3 = {_, Pub3} = ar_wallet:new(),
	GoodTX = sign_tx(Key1, #{data => <<>>}),
	ToDelTX1 = sign_tx(Key2, #{data => <<>>}),
	ToDelTX2 = sign_tx(Key3, #{data => <<>>}),
	Data = <<(ar_util:encode(ToDelTX1#tx.id))/binary, "\n", (ar_util:encode(ToDelTX2#tx.id))/binary>>,
	FullFilePath = ensure_content_policy_filename(),
	ok = file:write_file(FullFilePath, Data),
	UrlPath = "/mock/policy_content/" ++ FullFilePath,
	[Block] = ar_weave:init([{ar_wallet:to_address(Pub1), AR, <<>>}, {ar_wallet:to_address(Pub2), AR, <<>>}, {ar_wallet:to_address(Pub3), AR, <<>>}]),
	{Master, Block} = start(Block),
	assert_post_tx_to_master(Master, GoodTX),
	assert_post_tx_to_master(Master, ToDelTX1),
	assert_post_tx_to_master(Master, ToDelTX2),
	ar_node:mine(Master),
	wait_until_height(Master, 1),
	?assertEqual(ToDelTX1, ar_storage:read_tx(ToDelTX1#tx.id)),
	?assertEqual(ToDelTX2, ar_storage:read_tx(ToDelTX2#tx.id)),
	ar_meta_db:put(content_policy_provider_urls, ["http://localhost:1984" ++ UrlPath]),
	timer:sleep(5000),
	ok = file:delete(FullFilePath),
	?assertEqual(GoodTX, ar_storage:read_tx(GoodTX#tx.id)),
	?assertEqual(unavailable, ar_storage:read_tx(ToDelTX1#tx.id)),
	?assertEqual(unavailable, ar_storage:read_tx(ToDelTX2#tx.id)).

delete_tx_data_basic_aut_test_() ->
	{timeout, 60, fun test_delete_tx_data_with_basic_auth/0}.

test_delete_tx_data_with_basic_auth() ->
	{Master, _, Wallet} = ar_data_sync_tests:setup_nodes(),
	DataSize = 10000,
	OutOfBoundsOffsetChunk = crypto:strong_rand_bytes(DataSize),
	ChunkID = ar_tx:generate_chunk_id(OutOfBoundsOffsetChunk),
	{DataRoot, DataTree} = ar_merkle:generate_tree([{ChunkID, DataSize + 1}]),
	TX = sign_tx(
		Wallet,
		#{ last_tx => get_tx_anchor(master), data_size => DataSize, data_root => DataRoot }
	),
	post_and_mine(#{ miner => {master, Master}, await_on => {master, Master} }, [TX]),
	DataPath = ar_merkle:generate_path(DataRoot, 0, DataTree),
	Proof = #{
		data_root => ar_util:encode(DataRoot),
		data_path => ar_util:encode(DataPath),
		chunk => ar_util:encode(OutOfBoundsOffsetChunk),
		offset => <<"0">>,
		data_size => integer_to_binary(DataSize)
	},
	Data = <<(ar_util:encode(TX#tx.id))/binary, "\n">>,
	FullFilePath = ensure_content_policy_filename(),
	ok = file:write_file(FullFilePath, Data),
	UrlPath = "/mock/policy_content/auth/" ++ FullFilePath,
	?assertMatch(
		{ok, {{<<"200">>, _}, _, _, _, _}},
		ar_data_sync_tests:post_chunk(jiffy:encode(Proof))
	),
	?assertMatch(
		{ok, {{<<"200">>, _}, _, _, _, _}},
		ar_data_sync_tests:get_chunk(DataSize)
	),
	ar_meta_db:put(content_policy_provider_urls, ["http://user:pass@localhost:1984" ++ UrlPath]),
	timer:sleep(5000),
	ok = file:delete(FullFilePath),
	?assertMatch(
		{ok, {{<<"404">>, _}, _, _, _, _}},
		ar_data_sync_tests:get_chunk(DataSize)
	).

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

ensure_content_policy_filename() ->
	DirPath = filename:join(ar_meta_db:get(data_dir), "content_policy_provider"),
	ok = filelib:ensure_dir(DirPath ++ "/"),
	FileName = binary_to_list(<<(base64:encode(crypto:strong_rand_bytes(16)))/binary, ".txt">>),
	filename:join(DirPath, FileName).
