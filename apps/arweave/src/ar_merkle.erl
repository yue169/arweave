-module(ar_merkle).
-export([generate_tree/1, generate_path/3]).
-export([validate_path/3]).
-include_lib("eunit/include/eunit.hrl").

%%% Generates annotated merkle trees, paths inside those trees, as well 
%%% as verification of those proofs.

-record(node, {
    id,
    type = branch, % root | branch | leaf
    data, % The value (for leaves)
    note, % A number less than 2^64
    left, % The (optional) ID of a node to the left
    right, % The (optional) ID of a node to the right
    max % The maximum observed note at this point
}).

-define(HASH_SIZE, 32).
-define(NOTE_SIZE, 8).

%%% Tree generation.
%%% Returns the merkle root and the tree data structure.

generate_tree(Elements) ->
    generate_all_rows(generate_leaves(Elements)).

generate_leaves(Elements) ->
    {_MaxNote, LeavesRev} =
        lists:foldl(
            fun({Data, Note}, {AccNote, Nodes}) ->
                NewNote = Note + AccNote,
                Hash = hash([Data, note_to_binary(NewNote)]),
                {
                    NewNote,
                    insert(
                        #node {
                            id = Hash,
                            type = leaf,
                            data = Data,
                            note = NewNote,
                            max = NewNote
                        },
                        Nodes
                    )
                }
            end,
            {0, new()},
            Elements
        ),
    lists:reverse(LeavesRev).

generate_all_rows(Leaves) ->
    generate_all_rows(Leaves, Leaves).

generate_all_rows([RootN], Tree) ->
    {RootN#node.id, ar_util:unique(Tree)};
    generate_all_rows(Row, Tree) ->
    NewRow = generate_row(Row),
    generate_all_rows(NewRow, NewRow ++ Tree).

generate_row([]) -> [];
generate_row([Left]) -> [ generate_node(Left, empty) ];
generate_row([L, R | Rest]) ->
    [ generate_node(L, R) | generate_row(Rest) ].

generate_node(Left, empty) ->
    Left;
generate_node(L, R) ->
    #node {
        id = hash([L#node.id, R#node.id, note_to_binary(L#node.max)]),
        type = branch,
        left = L#node.id,
        right = R#node.id,
        note = L#node.max,
        max = R#node.max
    }.

%%% Merkle path generation and verification functions.

generate_path(ID, Dest, Tree) ->
    binary:list_to_bin(generate_path_parts(ID, Dest, Tree)).

generate_path_parts(ID, Dest, Tree) ->
    case get(ID, Tree) of
        N when N#node.type == leaf ->
            [ N#node.data, note_to_binary(N#node.note) ];
        N when N#node.type == branch ->
            [
                N#node.left, N#node.right, note_to_binary(N#node.note)
            |
                generate_path_parts(
                    case Dest =< N#node.note of
                        true -> N#node.left;
                        false -> N#node.right
                    end,
                    Dest,
                    Tree
                )
            ]
    end.

validate_path(ID, _Dest, << Data:?HASH_SIZE/binary, Note:(?NOTE_SIZE*8) >>) ->
    case hash([Data, note_to_binary(Note)]) of
        ID -> Data;
        _ -> false
    end;
validate_path(ID, Dest,
        << L:?HASH_SIZE/binary, R:?HASH_SIZE/binary, Note:(?NOTE_SIZE*8), Rest/binary >>) ->
    case hash([L, R, note_to_binary(Note)]) of
        ID ->
            validate_path(
                case Dest =< Note of
                    true -> L;
                    false -> R
                end,
                Dest,
                Rest
            );
        _ -> false
    end.

%%% Helper functions for managing the tree data structure.
%%% Abstracted so that the concrete data type can be replaced later.

new() ->
    [].

insert(Node, Map) ->
    [Node|Map].

get(ID, Map) ->
    case lists:keyfind(ID, #node.id, Map) of
        false -> false;
        Node -> Node
    end.

note_to_binary(Note) ->
    << Note:(?NOTE_SIZE * 8) >>.

hash(Parts) ->
    crypto:hash(sha256, binary:list_to_bin(Parts)).

%%% Tests

generate_balanced_tree_test() ->
    TestSize = 1024,
    {_MR, Tree} = ar_merkle:generate_tree([ {<<N:256>>, 1} || N <- lists:seq(1, TestSize) ]),
    ?assertEqual(length(Tree), (TestSize*2) - 1).

generate_and_validate_path_test() ->
    TestSize = 1024,
    {MR, Tree} = ar_merkle:generate_tree([ {<<N:256>>, 1} || N <- lists:seq(1, TestSize) ]),
    RandomTarget = rand:uniform(TestSize),
    ?assertEqual(
        RandomTarget,
        binary:decode_unsigned(
            ar_merkle:validate_path(
                MR, RandomTarget,
                ar_merkle:generate_path(MR, RandomTarget, Tree)
            )
        )
    ).