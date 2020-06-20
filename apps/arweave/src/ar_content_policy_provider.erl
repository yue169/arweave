-module(ar_content_policy_provider).

-behaviour(gen_server).

%%% ==================================================================
%%% gen_server API
%%% ==================================================================

-export([
	start_link/1
]).

%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================

-export([
	init/1,
	handle_call/3,
	handle_cast/2,
	terminate/2,
	process_policy_content/1
]).

%%% ==================================================================
%%% Includes
%%% ==================================================================

-include("ar.hrl").

%%% ==================================================================
%%% gen_server API functions
%%% ==================================================================

start_link(Args) ->
	gen_server:start_link({local, ?MODULE}, ?MODULE, Args, []).

%%% ==================================================================
%%% gen_server callbacks
%%% ==================================================================

init(_) ->
	timer:apply_after(?CONTENT_POLICY_SCAN_INTERVAL, gen_server, cast, [?MODULE, process_policy_content]),
	{ok, []}.

handle_call(_, _, State) ->
	{noreply, State}.

handle_cast(process_policy_content, State) ->
	timer:apply_after(?CONTENT_POLICY_SCAN_INTERVAL, gen_server, cast, [?MODULE, process_policy_content]),
	process_policy_content(ar_meta_db:get(content_policy_provider_urls, [])),
	{noreply, State};

handle_cast(_, State) ->
	{noreply, State}.

terminate(Reason, _) ->
	ar:err(Reason).

%%% ==================================================================
%%% Internal functions
%%% ==================================================================

process_policy_content([]) ->
	ok;
process_policy_content([URL|T]) ->
	case http_uri:parse(URL) of
		{ok, {_Scheme, UserInfo, Host, Port, Path, Query}} ->
			Peer = {Host, Port},
			FullPath = Path ++ Query,
			Headers = build_req_headers(UserInfo),
			Opts = #{method => get, peer => Peer, path => FullPath, headers => Headers, is_peer_request => false},
			case ar_http:req(Opts) of
				{ok, {{_ ,_}, _, Data, _, _}} ->
					BlacklistTxIDs = process_policy_content_txs(binary:split(Data, <<"\n">>, [global]), []),
					case ar_meta_db:get(content_policy_provider_tx_ids) of
						not_found ->
							ar_meta_db:put(content_policy_provider_tx_ids, BlacklistTxIDs);
						IDs ->
							ar_meta_db:put(content_policy_provider_tx_ids, lists:usort(BlacklistTxIDs ++ IDs))
					end;
				InvalidResponse ->
					ar:err([{event, process_policy_content_request}, {url, URL}, {response, InvalidResponse}])
			end;
		{error, Reason} ->
			ar:err([{event, process_policy_content_url}, {url, URL}, {reason, Reason}]),
			process_policy_content(T)
	end.

build_req_headers([]) ->
	[];
build_req_headers(UserInfo) ->
	[{<<"Authorization">>, <<"Basic ", (base64:encode(UserInfo))/binary>>}].

process_policy_content_txs([], Acc) ->
	Acc;
process_policy_content_txs([<<>>|T], Acc) ->
	process_policy_content_txs(T, Acc);
process_policy_content_txs([ID|T], Acc) ->
	TrimID = re:replace(ID, "[\s\t\r\n]", "", [{return, binary}, global]),
	case catch ar_util:decode(TrimID) of
		DecID when is_binary(DecID) ->
			ar_data_sync:delete_tx_data(DecID),
			ar_storage:delete_tx(DecID),
			process_policy_content_txs(T, [DecID|Acc]);
		_ ->
			process_policy_content_txs(T, Acc)
	end.
