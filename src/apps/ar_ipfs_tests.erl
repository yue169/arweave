-module(ar_ipfs_tests).
-include("../ar.hrl").
-include_lib("eunit/include/eunit.hrl").

-export([timestamp_data/1]).

pin_ls_cli_test() ->
	ar_ipfs:daemon_start(),
	PinsHTTP = lists:sort(ar_ipfs:pin_ls()),
	PinsCLI  = lists:sort(ar_ipfs:pin_ls(cli)),
	ar_ipfs:daemon_stop(),
	?assertEqual(PinsHTTP, PinsCLI).

pin_rm_cli_test() ->
	ar_ipfs:daemon_start(),
	Filename = "known_local.txt",
	DataDir = "src/apps/app_ipfs_test_data/",
	Path = DataDir ++ Filename,
	{ok, Data} = file:read_file(Path),
	DataToHash = timestamp_data(Data),
	{ok, Hash} = ar_ipfs:add_data(DataToHash, Filename),
	Pins1 = ar_ipfs:pin_ls(cli),
	ok = ar_ipfs:pin_rm(cli, Hash),
	timer:sleep(1000),
	Pins2 = ar_ipfs:pin_ls(cli),
	ar_ipfs:daemon_stop(),
	?assert(lists:member(Hash, Pins1)),
	?assertNot(lists:member(Hash, Pins2)).

pin_rm_test() ->
	ar_ipfs:daemon_start(),
	Filename = "known_local.txt",
	DataDir = "src/apps/app_ipfs_test_data/",
	Path = DataDir ++ Filename,
	{ok, Data} = file:read_file(Path),
	DataToHash = timestamp_data(Data),
	{ok, Hash} = ar_ipfs:add_data(DataToHash, Filename),
	Pins1 = ar_ipfs:pin_ls(),
	ok = ar_ipfs:pin_rm(Hash),
	timer:sleep(1000),
	Pins2 = ar_ipfs:pin_ls(),
	ar_ipfs:daemon_stop(),
	?assert(lists:member(Hash, Pins1)),
	?assertNot(lists:member(Hash, Pins2)).

get_everipedia_hashes_test_() ->
	{timeout, 60, fun() ->
		ar_ipfs:daemon_start(),
		N = 6,
		From = 240,
		{Hashes, _More} = app_ipfs_utils:ep_get_ipfs_hashes(N, From),
		lists:foreach(fun(H) -> io:format("Hash: ~p~n", [H]) end, Hashes),
		ar_ipfs:daemon_stop(),
		?assertEqual(N, length(Hashes))
	end}.

add_local_and_get_test() ->
	ar_ipfs:daemon_start(),
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
