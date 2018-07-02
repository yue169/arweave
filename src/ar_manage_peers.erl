-module(ar_manage_peers).
-export([start/0]).
-export([add_perf_data/6, reset_peer/1]).
-export([get_performance/1,format_stats/2]).
-export([update/1, stats/0, reset/0]).
-include_lib("eunit/include/eunit.hrl").
-include("ar.hrl").

%%% Manage and update peer lists.

start() ->
	ar:report([?MODULE]),
	ets:new(peer_performance, [set, public, named_table, {keypos, 2},
		{read_concurrency,  true}, {write_concurrency, true} ]),
	ok.


%% @doc Update the database with new performance data.
%
add_perf_data(Peer, Handler,Code, Sent, Recv, Time)  when is_binary(Code)->
	add_perf_data(Peer, Handler,binary_to_integer(Code), Sent, Recv, Time);
add_perf_data(Peer, _Handler, _Code, Sent, Recv, Time) ->
	ets:update_counter(peer_performance, Peer,[
		{3, 1, 1, os:system_time(seconds)}, %timeout - this is tricky we exploit Treshold to apply SetValue.
		{4, 1},       %requests
		{5, Sent},   %sent bytes (reqsize in client, and resp size in server)
		{6, Recv},   %recv bytes (other way around)
		{7, Time}   %time
		], #performance{peer=Peer}).

%% @doc Return the performance object for a node.
get_performance(Peer) ->
	case ets:lookup(peer_performance, Peer) of
		[] -> #performance{peer=Peer};
		[P] -> P;
		[P|_] -> P
	end.

%% @doc Print statistics about the current peers.
stats() ->
	Connected = ar_bridge:get_remote_peers(http_bridge_node),
	All = all_peers(),
	io:format("Connected peers, in preference order:~n"),
	stats(Connected),
	io:format("Other known peers:~n"),
	stats(All).
stats(Peers) ->
	lists:foreach(
		fun(Peer) -> format_stats(Peer, get_performance(Peer)) end,
		Peers
	).

%% @doc Update the "last on list" timestamp of a given peer
update_timer(Peer) ->
	ets:update_counter(peer_performance, Peer,[
		{3, 1, 1, os:system_time(seconds)} %timeout - this is tricky we exploit Treshold to apply SetValue.
		], #performance{peer=Peer}).

%% @doc Reset the performance data for a given peer.
reset_peer(Peer) ->
	ets:insert(peer_performance, #performance{peer=Peer}).

%% @doc Reset all performance counters and connections.
reset() ->
	lists:map(fun reset_peer/1, All = all_peers()),
	ar_bridge:set_remote_peers(whereis(http_bridge_node), All).

%% @doc Return all known peers.
all_peers() ->
	ets:foldl(fun(T, Acc) -> Acc ++ [T#performance.peer] end, [], peer_performance).

%% @doc Pretty print stats about a node.
format_stats(Peer, Perf) ->
	io:format("\t~s ~.2f kb/s (~p transfers)~n",
		[
			string:pad(ar_util:format_peer(Peer), 20, trailing, $ ),
			((Perf#performance.bytes_sent + Perf#performance.bytes_recv) / 1024) / (Perf#performance.time + 1 / 1000000),
			Perf#performance.transfers
		]
	).

%% @doc Take an existing peer list and create a new peer list. Gets all current peers
%% peerlist and ranks each peer by its connection speed to this node in the past.
%% Peers who have behaved well in the past are favoured in ranking.
%% New, unknown peers are given 100 blocks of grace.
update(Peers) ->
	remove_old(os:system_time(seconds)),
	{Rankable, Newbies} = 
		partition_newbies(
			score(
				lists:filter(
					fun(P) ->
						(not lists:member(P, ?PEER_PERMANENT_BLACKLIST)) and responds(P)
					end,
					get_more_peers(Peers)
				)
			)
		),
	NewPeers = (lists:sublist(maybe_drop_peers([ Peer || {Peer, _} <- rank_peers(Rankable) ]), ?MAXIMUM_PEERS)
		++ [ Peer || {Peer, newbie} <- Newbies ]),
	lists:foreach(
		fun(P) ->
			case lists:member(P, NewPeers) of
				false -> update_timer(P);
				_ -> ok
			end
		end,
		Peers	
	),
	NewPeers.

%% @doc Return a new list, with the peers and their peers.
get_more_peers(Peers) ->
	ar_util:unique(
		lists:flatten(
			[
				ar_util:pmap(fun ar_http_iface:get_peers/1, Peers),
				Peers
			]
		)
	).

responds(Peer) ->
	not (is_atom(ar_http_iface:get_info(Peer))).

%% @doc Calculate a rank order for any given peer or list of peers.
score(Peers) when is_list(Peers) ->
	lists:map(fun(Peer) -> {Peer, score(Peer)} end, Peers);
score(Peer) ->
	case get_performance(Peer) of
		P when P#performance.transfers < ?PEER_GRACE_PERIOD ->
			newbie;
		P -> (P#performance.bytes_sent + P#performance.bytes_recv) / (P#performance.time + 1)
	end.

%% @doc Given a set of peers, returns a tuple containing peers that
%% are "rankable" and elidgible to be pruned, and new peers who are
%% within their grace period who are not
partition_newbies(ScoredPeers) ->
	Newbies = [ P || P = {_, newbie} <- ScoredPeers ],
	{ScoredPeers -- Newbies, Newbies}.

%% @doc Return a ranked list of peers.
rank_peers(ScoredPeers) ->
	lists:sort(fun({_, S1}, {_, S2}) -> S1 >= S2 end, ScoredPeers).

%% @doc Probabalistically drop peers based on their rank. Highly ranked peers are
%% less likely to be dropped than lower ranked ones.
maybe_drop_peers(Peers) -> maybe_drop_peers(1, length(Peers), Peers).
maybe_drop_peers(_, _, []) -> [];
maybe_drop_peers(Rank, NumPeers, [Peer|Peers]) when Rank =< ?MINIMUM_PEERS ->
	[Peer|maybe_drop_peers(Rank + 1, NumPeers, Peers)];
maybe_drop_peers(Rank, NumPeers, [Peer|Peers]) ->
	case roll(Rank, NumPeers) of
		true -> [Peer|maybe_drop_peers(Rank + 1, NumPeers, Peers)];
		false -> maybe_drop_peers(Rank + 1, NumPeers, Peers)
	end.

%% @doc Generate a boolean 'drop or not' value from a rank and the number of peers.
roll(Rank, NumPeers) ->
	case Rank =< ?MINIMUM_PEERS of
		true -> true;
		false -> 
			(2 * rand:uniform(NumPeers - ?MINIMUM_PEERS)) >=
				(Rank - ?MINIMUM_PEERS)
	end.

%% @doc Remove entries from the performance database older than
%% ?PEER_TMEOUT
remove_old(Time) ->
	%PeersToPurge= ets:select(peer_performance, [
	%	{{'_','$2','$3','_','_','_','_','_'},[{'<','$3',Time - ?PEER_TIMEOUT }],['$2']}]),
	%ar:report([{ar_manage_peers,add_perf_data}, {peers_to_purge, PeersToPurge}]),
	ets:select_delete(peer_performance, [
		{{'_','$2','$3','_','_','_','_','_'},[{'<','$3',Time - ?PEER_TIMEOUT }],[true]}]).

%% @doc Data older than ?PEER_TIMEOUT is removed, newer data is not
purge_old_peers_test() ->
	Time = os:system_time(seconds),
	P1 = #performance{peer={127,0,0,1,1984}, timeout = Time - (?PEER_TIMEOUT + 1)},
	P2 = #performance{peer={127,0,0,1,1985}, timeout = Time - 1},
	ets:insert(peer_performance,  P1),
	ets:insert(peer_performance,  P2),
	remove_old(Time),
	?assertEqual([], ets:lookup(peer_performance,{127,0,0,1,1984})),
	?assertEqual([P2], ets:lookup(peer_performance, {127,0,0,1,1985})).
