-module(ar_unit_testing).
-export([run_all_tests/0]).
-include_lib("eunit/include/eunit.hrl").

%%% Configures eunit to avoid timeouts.

%TODO write docs

%% @doc Timeout in seconds.
-define(TIMEOUT, 600).

%% @doc Run all unit tests.
run_all_tests() ->
	run_module_tests(get_modules()).

%% @doc
get_tests_from_module(M) ->
	[ X || {X, _} <- M:module_info(functions), lists:suffix("test", atom_to_list(X)) ].

run_module_tests([]) -> ok;
run_module_tests([M|Ms]) ->
	Ts = get_tests_from_module(M),
	run_tests(M, Ts),
	run_module_tests(Ms).

%run_tests(M, Ts) ->
%	eunit:test(
%		[ {timeout, ?TIMEOUT, fun M:T/0} || T <- Ts ]
%	).
run_tests(_, []) -> ok;
run_tests(M, [T|Ts]) ->
	eunit:test( {timeout, ?TIMEOUT, fun M:T/0}),
	run_tests(M, Ts).

get_modules() ->
	[
		ar_tx,
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
		ar_util,
		ar_wallet,
		ar_weave
	].
