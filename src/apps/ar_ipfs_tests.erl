-module(ar_ipfs_tests).
-include("../ar.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([timestamp_data/1]).

get_everipedia_hashes_test_() ->
	{timeout, 60, fun() ->
		ar_ipfs:daemon_start(),
		timer:sleep(1000),
		N = 6,
		From = 240,
		{Hashes, _More} = ar_ipfs:ep_get_ipfs_hashes(N, From),
		lists:foreach(fun(H) -> io:format("Hash: ~p~n", [H]) end, Hashes),
		ar_ipfs:daemon_stop(),
		?assertEqual(N, length(Hashes))
	end}.

add_local_and_get_test() ->
	ar_ipfs:daemon_start(),
	timer:sleep(1000),
	Filename = "known_local.txt",
	DataDir = "src/apps/app_ipfs_test_data/",
	Path = DataDir ++ Filename,
	{ok, Data} = file:read_file(Path),
	DataToHash = timestamp_data(Data),
	{ok, Hash} = ar_ipfs:add_data(DataToHash, Filename),
	{ok, ActualDataToHash} = ar_ipfs:cat_data_by_hash(Hash),
	ar_ipfs:daemon_stop(),
	?assertEqual(DataToHash, ActualDataToHash).

%%% private

timestamp_data(Data) ->
	timestamp_data(ar_ipfs:rfc3339_timestamp(), Data).
timestamp_data(TS, Data) ->
	<<TS/binary, "  *  ", Data/binary>>.
