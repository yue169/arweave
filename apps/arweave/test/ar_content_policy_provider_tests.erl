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

%% By pastebin.com was detected a lot of requests, by this reason, can be run one of few tests
%% Looks like we need a mock file server behavior
%removes_blacklisted_txs_test_() ->
%	{timeout, 60, fun removes_blacklisted_txs/0}.

removes_blacklisted_txs() ->
	AR = ?AR(20),
	Key1 = {_, Pub1} = ar_wallet:new(),
	Key2 = {_, Pub2} = ar_wallet:new(),
	Key3 = {_, Pub3} = ar_wallet:new(),
	GoodTX = sign_tx(Key1, #{data => <<>>}),
	ToDelTX1 = sign_tx(Key2, #{data => <<>>}),
	ToDelTX2 = sign_tx(Key3, #{data => <<>>}),
	Data = <<(ar_util:encode(ToDelTX1#tx.id))/binary, "\n", (ar_util:encode(ToDelTX2#tx.id))/binary>>,
	URL = pastebin(Data),
	[Block] = ar_weave:init([{ar_wallet:to_address(Pub1), AR, <<>>}, {ar_wallet:to_address(Pub2), AR, <<>>}, {ar_wallet:to_address(Pub3), AR, <<>>}]),
	{Master, Block} = start(Block),
	assert_post_tx_to_master(Master, GoodTX),
	assert_post_tx_to_master(Master, ToDelTX1),
	assert_post_tx_to_master(Master, ToDelTX2),
	ar_node:mine(Master),
	wait_until_height(Master, 1),
	?assertEqual(ToDelTX1, ar_storage:read_tx(ToDelTX1#tx.id)),
	?assertEqual(ToDelTX2, ar_storage:read_tx(ToDelTX2#tx.id)),
	ar_meta_db:put(content_policy_provider_urls, [URL]),
	timer:sleep(5000),
	?assertEqual(GoodTX, ar_storage:read_tx(GoodTX#tx.id)),
	?assertEqual(unavailable, ar_storage:read_tx(ToDelTX1#tx.id)),
	?assertEqual(unavailable, ar_storage:read_tx(ToDelTX2#tx.id)).

delete_tx_data_test_() ->
	{timeout, 60, fun test_delete_tx_data/0}.

test_delete_tx_data() ->
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
	URL = pastebin(Data),
	?assertMatch(
		{ok, {{<<"200">>, _}, _, _, _, _}},
		ar_data_sync_tests:post_chunk(jiffy:encode(Proof))
	),
	?assertMatch(
		{ok, {{<<"200">>, _}, _, _, _, _}},
		ar_data_sync_tests:get_chunk(DataSize)
	),
	ar_meta_db:put(content_policy_provider_urls, [URL]),
	timer:sleep(5000),
	?assertMatch(
		{ok, {{<<"404">>, _}, _, _, _, _}},
		ar_data_sync_tests:get_chunk(DataSize)
	).

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

pastebin(Data) ->
	% api_dev_key:
	% e2da95f2fffd50e447615243513f54e3
	% 69dc2518b1d28b058cd0b57f8e4990ef
	% 5f0067cbd866d9cff35644cfdb31b0bd
	% 2a9e041066eec7573d0558529e0fc6a2
	Curl = <<"curl -X POST -H \"Content-Type: application/x-www-form-urlencoded; charset=UTF-8\" --data \' api_dev_key=2a9e041066eec7573d0558529e0fc6a2&api_option=paste&api_paste_private=0&api_paste_name=test&api_paste_expire_date=10M&api_paste_format=text&api_paste_code=", Data/binary,"\' \'https://pastebin.com/api/api_post.php\'">>,
	Res = os:cmd(binary_to_list(Curl)),
	SpitRes = re:split(Res, "\n", [{return, list}]),
	FileURL = lists:last(SpitRes),
	re:replace(FileURL, "com/", "com/raw/", [{return, list}]).
