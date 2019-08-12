-module(app_ipfs_utils).

-export([send/2]).
-include("ar.hrl").

send(Filename, Wallet) ->
	{ok, Data} = file:read_file(Filename),
	Reward = calc_reward(Data),
	{ok, LastTXid} = ar_node:get_last_tx(whereis(http_entrypoint_node), Wallet),
	TX = #tx{
		data=Data,
		reward=Reward,
		last_tx=LastTXid,
		tags=[{<<"IPFS-Add">>, Filename}]
	},
	SignedTX = ar_tx:sign(TX, Wallet),
	ar_http_iface_client:send_new_tx({127, 0, 0, 1, 1984}, SignedTX).

%%%% private

calc_reward(Data) ->
	DataSize = byte_size(Data),
	Diff = ar_node:get_current_diff(whereis(http_entrypoint_node)) - 1,
	Height = ar_node:get_height(whereis(http_entrypoint_node)),
	ar_tx:calculate_min_tx_cost(DataSize, Diff, Height).
