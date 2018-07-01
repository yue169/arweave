-module(ar_httpc).
-export([request/1, request/4, request/5]).
-include("ar.hrl").

%%% A wrapper library for httpc.
%%% Performs HTTP calls and stores the peer and time-per-byte
%%% in the meta db.

%% @doc Perform a HTTP call with the httpc library, store the time required.
request(Peer) ->
	request(<<"GET">>, Peer, <<"/">>, <<>>, ?NET_TIMEOUT).

request(Method, Peer, Path, Body) ->
	request(Method, Peer, Path, Body, ?NET_TIMEOUT).

request(Method, Peer, Path, Body, Timeout) ->
	%ar:report([{ar_httpc_request,Peer},{method,Method}, {path,Path}]),
	Host="http://" ++ ar_util:format_peer(Peer),
	{ok, Client} = fusco:start(Host, [{connect_timeout, ?CONNECT_TIMEOUT}]),
	Result = fusco:request(Client, list_to_binary(Path), Method, [], Body, 1, Timeout),
	ok = fusco:disconnect(Client),
	case Result of
		{ok, {{Code, _Reason}, _Hdrs, RespBody, Size, Time}} ->
			ar_manage_peers:add_perf_data(Peer, client, Code,
				byte_size(Body),byte_size(RespBody), Time);
		%%{error, Reason} -> TODO
		_ -> ok
		end,
	Result.

