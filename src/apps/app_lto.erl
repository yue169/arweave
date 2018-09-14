-module(app_lto).
-export([start/0, new_block/1]).
-include("ar.hrl").
%-define(NODE_ADDR, "127.0.0.1:6863").
-define(NODE_ADDR, "lto-anchor.eu-west-1.elasticbeanstalk.com:80").

%%% Integration with the LTO network.
%%% Checkpoints the hash of each newly received Arweave block on
%%% the LTO public network.
%%% Requires an unauthenticated LTO node with a filled wallet.
%%% Remember: Firewall the LTO node from inappropriate access!

%% @doc Start the server as an adt_simple app, calling back to this module.
start() ->
    inets:start(),
    ssl:start(),
	adt_simple:start(?MODULE).

%% @doc This function is called and passed a new block every time one is added
%% to the blockweave.
new_block(B) ->
    send_hash(B#block.indep_hash).

%% @doc Prepare and send a POST request to the local LTO node.
%% NOTE: Hashes are truncated to 256 bits.
send_hash(<< Hash256:256/bitstring, _/bitstring >>) ->
    httpc:request(
        post,
        {
            "http://" ++ ?NODE_ADDR ++ "/hash",
            [],
            "application/json",
            jiffy:encode(
                {
                    [
                        {<<"hash">>, base64:encode(Hash256)},
                        {<<"encoding">>, <<"base64">>}
                    ]
                }
            )
            },
        [],
        []
    ).