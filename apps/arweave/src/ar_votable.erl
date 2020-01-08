-module(ar_votable).
-export([init/0, get/2, validate/2, vote/1]).
-include("ar.hrl").

%%% Interfaces for generating and interacting with votable options in
%%% the network. Introduced in v2.0.

-record(property, {
    name, % String
    max_up, % Maximum percentage change up per block
    max_down, % Maximum percent change down per block
    default % Default value
}).

%% Transform a voted number in and out of the block-compatible format.
-define(TO_INT(X), erlang:trunc(X * 1000000000000)).
-define(TO_NUM(X), (X / 1000000000000)).

properties() ->
    [
        #property {
            name = "vote_power",
            max_up = 0.01,
            max_down = 0.1,
            default = ?TO_INT(1)
        },
        #property {
            name = "txs_per_block",
            max_up = 0.01,
            max_down = 0.02,
            default = ?TO_INT(10000)
        }
    ].

%% @doc Generate base votables for storage after the fork block in which
%% they are initiated. At the moment all votables are introduced in v2.0.
init() ->
    [
        {P#property.name, P#property.default}
    ||
        P <- properties()
    ].

%% @doc Return the current value for a votable property, given a block.
get(Name, B) when is_record(B, block) ->
    get(Name, B#block.votables);
get(Atom, Votables) when is_atom(Atom) ->
    get(atom_to_list(Atom), Votables);
get(Name, Votables) ->
    case lists:keyfind(Name, 1, Votables) of
        false ->
            {Name, Value} = lists:keyfind(Name, 1, properties()),
            ?TO_NUM(Value);
        {Name, Value} ->
            ?TO_NUM(Value)
    end.

%% @doc Validate that a new block's votables are within bounds of the
%% previous block.
validate(NewB, OldB) ->
    VotePower = get("vote_power", OldB),
    lists:all(
        fun({Name, NewValue}) ->
            P = lists:keyfind(Name, #property.name, properties()),
            PrevValue = get(Name, OldB),
            {Low, High} =
                calculate_bounds(
                    PrevValue,
                    VotePower,
                    P#property.max_up,
                    P#property.max_down
                ),
            (NewValue >= Low) and (NewValue =< High)
        end,
        NewB#block.votables
    ).

%% @doc Given the current voting power and property-specific possible
%% change per block, calculate the maximum and minimum bounds in a new block.
calculate_bounds(Num, VotePower, DownMax, UpMax) ->
    {
        Num - (Num * (VotePower * (UpMax / 100))),
        Num + (Num * (VotePower * (DownMax / 100)))
    }.

%% @doc Move votable values the maximum distance towards the node's prefered
%% values.
vote(Votables) ->
    VotePower = get("vote_power", Votables),
    lists:map(
        fun({Name, BlockValue}) ->
            Value = ?TO_NUM(BlockValue),
            P = lists:keyfind(Name, #property.name, properties()),
            {Low, High} =
                calculate_bounds(
                    ?TO_NUM(BlockValue),
                    VotePower,
                    P#property.max_up,
                    P#property.max_down
                ),
            case ar_meta_db:get({votable, Name}) of
                false ->
                    {Name, BlockValue};
                DesiredValue when DesiredValue > Value ->
                    {Name, ?TO_INT(erlang:min(DesiredValue, High))};
                DesiredValue when DesiredValue =< Value ->
                    {Name, ?TO_INT(erlang:max(DesiredValue, Low))}
            end
        end,
        Votables
    ).