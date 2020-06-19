-module(ar_content_policy_provider_tests).

-include("src/ar.hrl").
-include_lib("eunit/include/eunit.hrl").

-import(ar_test_node, [start/1, sign_tx/2]).
-import(ar_test_node, [assert_post_tx_to_master/2, wait_until_height/2]).

removes_blacklisted_txs_test_() ->
	{timeout, 240, fun removes_blacklisted_txs/0}.

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

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

pastebin(Data) ->
	% api_dev_key:
	% e2da95f2fffd50e447615243513f54e3
	% 69dc2518b1d28b058cd0b57f8e4990ef
	Curl = <<"curl -X POST -H \"Content-Type: application/x-www-form-urlencoded; charset=UTF-8\" --data \' api_dev_key=e2da95f2fffd50e447615243513f54e3&api_option=paste&api_paste_private=0&api_paste_name=test&api_paste_expire_date=10M&api_paste_format=text&api_paste_code=", Data/binary,"\' \'https://pastebin.com/api/api_post.php\'">>,
	Res = os:cmd(binary_to_list(Curl)),
	SpitRes = re:split(Res, "\n", [{return, list}]),
	FileURL = lists:last(SpitRes),
	re:replace(FileURL, "com/", "com/raw/", [{return, list}]).
