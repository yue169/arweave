-module(app_ipfs_utils).

-export([send/2, send/3]).
-export([ep_get_ipfs_hashes/2]). % only used in tests
-include("ar.hrl").

send(Filename, Wallet, LastTXidEnc) ->
	{ok, Data} = file:read_file(Filename),
	{ok, Hash} = ar_ipfs:add_data(Data, Filename),
	Reward = ?AR(0.0010),
	LastTXid = ar_util:decode(LastTXidEnc),
	TX = #tx{
		data=Data,
		reward=Reward,
		last_tx=LastTXid,
		tags=[{<<"IPFS-Add">>, Hash}]
	},
	SignedTX = ar_tx:sign(TX, Wallet),
	ar_http_iface_client:send_new_tx({127, 0, 0, 1, 1984}, SignedTX).

send(Filename, Wallet) ->
	{ok, Data} = file:read_file(Filename),
	{ok, Hash} = ar_ipfs:add_data(Data, Filename),
	Reward = calc_reward(Data),
	{ok, LastTXid} = ar_node:get_last_tx(whereis(http_entrypoint_node), Wallet),
	TX = #tx{
		data=Data,
		reward=Reward,
		last_tx=LastTXid,
		tags=[{<<"IPFS-Add">>, Hash}]
	},
	SignedTX = ar_tx:sign(TX, Wallet),
	ar_http_iface_client:send_new_tx({127, 0, 0, 1, 1984}, SignedTX).

ep_get_ipfs_hashes(N, From) ->
	{ok, _} = application:ensure_all_started(ssl),
	URL = "https://mainnet.libertyblock.io:7777/v1/chain/get_table_rows",
	Headers = [],
	ContentType = [],
	ReqProps = [
		{scope, <<"eparticlectr">>},
		{code, <<"eparticlectr">>},
		{table, <<"wikistbl">>},
		{json, true},
		{lower_bound, From},
		{limit, N}
	],
	Body = jiffy:encode({ReqProps}),
	{ok, {_, _, RespBin}} = httpc:request(post, {URL, Headers, ContentType, Body}, [], []),
	Response = list_to_binary(RespBin),
	{RespProps} = jiffy:decode(Response),
	MaybeMore = case lists:keyfind(<<"more">>, 1, RespProps) of
		{<<"more">>, More} -> More;
		false              -> false
	end,
	HashTups = case lists:keyfind(<<"rows">>, 1, RespProps) of
		false              -> [];
		{<<"rows">>, Rows} -> lists:map(fun row_to_hash_tup/1, Rows)
	end,
	{HashTups, MaybeMore}.

%%%% private

calc_reward(Data) ->
	DataSize = byte_size(Data),
	Diff = ar_node:get_current_diff(whereis(http_entrypoint_node)) - 1,
	Height = ar_node:get_height(whereis(http_entrypoint_node)),
	ar_tx:calculate_min_tx_cost(DataSize, Diff, Height).

row_to_hash_tup({Props}) ->
	case lists:sort(Props) of
		[{<<"hash">>, Hash},{<<"id">>, Id},{<<"parent_hash">>, PHash}] ->
			{Id, Hash, PHash};
		_ ->
			{none, <<>>, <<>>}
	end.
