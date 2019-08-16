-module(ar_ipfs).
-export([daemon_is_running/0, daemon_start/0, daemon_stop/0, daemon_stop/2]).
-export([add_data/2, add_data/4, add_file/1, add_file/3]).
-export([cat_data_by_hash/1, cat_data_by_hash/3]).
-export([config_get_identity/0, config_set_identity/1]).
-export([dht_provide_hash/1, dht_provide_hash/3]).
-export([key_gen/1, key_gen/3]).
-export([pin_ls/0, pin_ls/2]).
-export([pin_rm/1, pin_rm/3]).
-export([rfc3339_timestamp/0, rfc3339_timestamp/1]).

-define(BOUNDARY, "------------qwerasdfzxcv").
-define(IPFS_HOST, "127.0.0.1").
-define(IPFS_PORT, "5001").

-type hash() :: binary().

%% "wishlist: way to ensure ipfs daemon is running"
%% "How to tell if succeede to start ipfs daemon?"
%% https://github.com/ipfs/go-ipfs/issues/5983
daemon_is_running() ->
	case lists:prefix(
			"Error: this action must be run in online mode",
			os:cmd("ipfs swarm peers")
		) of
		false -> true;
		true  -> false
	end.

daemon_start() ->
	case daemon_is_running() of
		true -> ok;
		false  ->
			spawn(os, cmd, ["ipfs daemon"]),
			timer:sleep(2000),
			ok
	end.

daemon_stop() ->
	daemon_stop(?IPFS_HOST, ?IPFS_PORT).

daemon_stop(IP, Port) ->
	Path = "/api/v0/shutdown",
    URL = "http://" ++ IP ++ ":" ++ Port ++ Path,
    {ok, _} = request(post, {URL, [], "", ""}, Path).

rfc3339_timestamp() ->
	rfc3339_timestamp({utc, calendar:universal_time()}).

rfc3339_timestamp({utc, UTCDateTime}) ->
	{{Year, Month, Day}, {Hours, Min, Sec}} = UTCDateTime,
	iolist_to_binary(
		io_lib:format(
			"~4..0B-~2..0B-~2..0BT~2..0B:~2..0B:~2..0BZ",
			[Year, Month, Day, Hours, Min, Sec]
		)
	).

add_data(Data, Filename) ->
	add_data(?IPFS_HOST, ?IPFS_PORT, Data, Filename).

add_data(IP, Port, DataB, FilenameB) ->
	Path = "/api/v0/add?pin=true",
	URL = "http://" ++ IP ++ ":" ++ Port ++ Path,
	Data = binary_to_list(DataB),
	Filename = thing_to_list(FilenameB),
	Boundary = ?BOUNDARY,
	Body = format_multipart_formdata(Boundary, [{Filename, Data}]),
	ContentType = lists:concat(["multipart/form-data; boundary=", Boundary]),
	Headers = [{"Content-Length", integer_to_list(length(Body))}],
	case request(post, {URL, Headers, ContentType, Body}, Path) of
		{ok, Response} ->
			{Props} = response_to_json(Response),
			{<<"Hash">>, Hash} = lists:keyfind(<<"Hash">>, 1, Props),
			{ok, Hash};
		{error, Reason} ->
			{error, Reason}
	end.

add_file(Path) ->
	add_file(?IPFS_HOST, ?IPFS_PORT, Path).

add_file(IP, Port, Path)->
    {ok, Data} = file:read_file(Path),
	Filename = filename:basename(Path),
	add_data(IP, Port, Data, Filename).

cat_data_by_hash(Hash) ->
	cat_data_by_hash(?IPFS_HOST, ?IPFS_PORT, Hash).

cat_data_by_hash(IP, Port, Hash) ->
	URL = "http://" ++ IP ++ ":" ++ Port ++ "/api/v0/cat?arg=" ++ binary_to_list(Hash),
	request(get, {URL, []}, "/api/v0/cat").

-spec config_get_identity() -> string().
config_get_identity() ->
	lists:droplast(os:cmd("ipfs config Identity.PeerID")).

-spec config_set_identity(hash()) -> ok.
config_set_identity(Key) ->
	os:cmd("ipfs config Identity.PeerID " ++ thing_to_list(Key)),
	ok.

dht_provide_hash(IPFSHash) ->
	dht_provide_hash(?IPFS_HOST, ?IPFS_PORT, IPFSHash).

dht_provide_hash(IP, Port, IPFSHash) ->
    URL = "http://" ++ IP ++ ":" ++ Port ++ "/api/v0/dht/provide?arg="
		++ thing_to_list(IPFSHash),
	ar:d({ar_ipfs, dht_provide_hash, IPFSHash}),
	request(get, {URL, []}, "/api/v0/dht/provide").

-spec key_gen(string()) -> {ok, hash()} | {error, list()}.
key_gen(Name) ->
	key_gen(?IPFS_HOST, ?IPFS_PORT, Name).

-spec key_gen(string(), string(), string()) -> {ok, binary()} | {error, list()}.
key_gen(IP, Port, Name) ->
	URL = "http://" ++ IP ++ ":" ++ Port ++ "/api/v0/key/gen?type=rsa&arg=" ++ Name,
	{ok, Response} = request(get, {URL, []}, "/api/v0/key/gen?type=rsa"),
	{Props} = response_to_json(Response),
	case lists:keyfind(<<"Id">>, 1, Props) of
		{<<"Id">>, Key} -> {ok, Key};
		false           -> {error, Props}
	end.

pin_ls() ->
	pin_ls(?IPFS_HOST, ?IPFS_PORT).

pin_ls(IP, Port) ->
	Path = "/api/v0/pin/ls",
    URL = "http://" ++ IP ++ ":" ++ Port ++ Path,
    {ok, Response} = request(post, {URL, [], [], ""}, Path),
	{[{<<"Keys">>, {Props}}]} = response_to_json(Response),
	Hashes = [K || {K, _} <- Props],
	Hashes.

pin_rm(IPFSHash) ->
	pin_rm(?IPFS_HOST, ?IPFS_PORT, IPFSHash).

pin_rm(IP, Port, IPFSHash) ->
	IHS = thing_to_list(IPFSHash),
	Path = "/api/v0/pin/rm",
    URL = "http://" ++ IP ++ ":" ++ Port ++ Path ++ "?arg=" ++ IHS ++ "&recursive=true",
    {ok, Response} = request(post, {URL, [], [], ""}, Path),
	%{[{<<"Pins">>, {Props}}]} = response_to_json(Response),
	%Hashes = [K || {K, _} <- Props],
	ok.

%%% private

format_multipart_formdata(Boundary,  Files) ->
	FileParts = lists:append(lists:map(
					fun({FileName, FileContent}) ->
							[lists:concat(["--", Boundary]),
							lists:concat(["Content-Disposition: file; name=\"","path","\"; filename=\"",FileName,"\""]),
							lists:concat(["Content-Type: ", "application/octet-stream"]),
							"",
							FileContent]
					end,
					Files)),
	Suffix = [lists:concat(["--", Boundary, "--"]), ""],
	Parts = lists:append([FileParts, Suffix]),
	string:join(Parts, "\r\n").

request(Method, Request, Label) ->
    Response = httpc:request(Method, Request, [{timeout, 3000}], []),
	case Response of
		{ok, {_, _, Body}} ->
			{ok, list_to_binary(Body)};
		Error ->
			ar:report({?MODULE, error, Label, Error}),
			% example errors:
			% {error,{failed_connect,[{to_address,{"127.0.0.1",5001}},
			%                         {inet,[inet],econnrefused}]}}
			{error, Error}
	end.

response_to_json(Response) ->
	jiffy:decode(Response).

thing_to_list(X) when is_list(X) -> X;
thing_to_list(X) when is_binary(X) -> binary_to_list(X).
