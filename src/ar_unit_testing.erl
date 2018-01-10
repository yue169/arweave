-module(ar_unit_testing).
-export([run_all_tests/0]).
-include_lib("eunit/include/eunit.hrl").

%%% Configures eunit to avoid timeouts.

%% @doc Timeout in seconds.
-define(TIMEOUT, 600).

run_all_tests() ->
	Modules = get_modules(),
	run_module_tests(Modules).

generate_test(ModuleName, FunctionName) ->
	{generator, ModuleName, list_to_atom(FunctionName)}.

get_tests_from_module(M) ->
	Fs = M:module_info(functions),
	FNames = lists:map(fun({X,_}) -> atom_to_list(X) end, Fs),
	[ X || X <- FNames, lists:suffix("test", X) ].

run_module_tests([M]) ->
	Ts = get_tests_from_module(M),
	run_tests(M, Ts);
run_module_tests([M|Ms]) ->
	Ts = get_tests_from_module(M),
	run_tests(M, Ts),
	run_module_tests(Ms).

run_tests(M, [T]) ->
	eunit:test( {timeout, ?TIMEOUT, generate_test(M,T)});
run_tests(M, [T|Ts]) ->
	eunit:test( {timeout, ?TIMEOUT, generate_test(M,T)}),
	run_tests(M, Ts).

get_modules() ->
	[
		ar_fork_recovery,
		ar_gossip,
		ar_http_iface,
		ar_join,
		ar_logging,
		ar_meta_db,
		ar_mine,
		ar_network,
		ar_node,
		ar_poller,
		ar_retarget,
		ar_router,
		ar_serialize,
		ar_services,
		ar_sim_client,
		ar_simple_reporter,
		ar_storage,
		ar_test_monitor,
		ar_test_sup,
		ar_tx,
		ar_util,
		ar_wallet,
		ar_weave
	].
