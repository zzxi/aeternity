-module(aest_large_cluster_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Test cases
-export([peers_propagation/1]).

-import(aest_nodes, [
    setup_nodes/2,
    start_node/2,
    stop_node/2, stop_node/3,
    kill_node/2,
    connect_node/3, disconnect_node/3,
    http_get/5,
    request/4,
    wait_for_value/4,
    assert_synchronized/2,
    get_node_pubkey/2
]).

%=== INCLUDES ==================================================================

-include_lib("eunit/include/eunit.hrl").

%=== MACROS ====================================================================

-define(MINING_TIMEOUT, 4000).
-define(STARTUP_TIMEOUT, 8000).


%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    peers_propagation
].

init_per_testcase(_TC, Config) ->
    %% Some parameters depend on the speed and capacity of the docker containers:
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

%=== TEST CASES ================================================================

peers_propagation(Cfg) ->
    NodeCount = 20,

    AllNodes = [node_name(I) || I <- lists:seq(1, NodeCount)],
    FirstHalf = [node_name(I) || I <- lists:seq(1, NodeCount div 2)],
    SecondHalf = [node_name(I) || I <- lists:seq(NodeCount div 2 + 1, NodeCount)],

    FirstHalfSpecs = [#{
        name    => N,
        peers   => [node1],
        backend => aest_docker,
        source  => {pull, "aeternity/epoch:local"},
        debug   => true
    } || N <- FirstHalf],

    SecondHalfSpecs = [#{
        name    => N,
        peers   => [pick_random(AllNodes)],
        backend => aest_docker,
        source  => {pull, "aeternity/epoch:local"},
        debug   => true
    } || N <- SecondHalf],

    NodeSpecs = FirstHalfSpecs ++ SecondHalfSpecs,
    setup_nodes(NodeSpecs, Cfg),

    NodeKeys = maps:from_list([{N, get_node_pubkey(N, Cfg)} || N <- AllNodes]),

    start_node(FirstHalf, Cfg),
    wait_for_value({height, 0}, FirstHalf, ?STARTUP_TIMEOUT, Cfg),
    wait_for_value({height, 50}, FirstHalf, ?MINING_TIMEOUT * 50, Cfg),

    %% Check all nodes know about all other nodes
    check_peers(FirstHalf, NodeKeys, Cfg),

    wait_for_value({height, 70}, FirstHalf, ?MINING_TIMEOUT * 20, Cfg),

    %% Check the chain is not forked
    check_not_forked(FirstHalf, 70, Cfg),

    %% More nodes join the network
    start_node(SecondHalf, Cfg),
    wait_for_value({height, 0}, SecondHalf, ?STARTUP_TIMEOUT, Cfg),
    wait_for_value({height, 120}, AllNodes, ?MINING_TIMEOUT * 50, Cfg),

    %% Check all nodes know about all other nodes
    check_peers(AllNodes, NodeKeys, Cfg),

    %% Check the chain is not forked
    check_not_forked(AllNodes, 120, Cfg),

    ok.

%=== INTERNAL FUNCTIONS ========================================================

node_name(I) -> list_to_atom("node" ++ integer_to_list(I)).

pick_random(List) ->
    Index = random:uniform(length(List)),
    lists:nth(Index, List).

check_peers(NodeNames, NodeKeys, Cfg) ->
    lists:map(fun(NodeName) ->
        {ok, 200, #{peers := PeersAddr}} =
            http_get(NodeName, int_http, [v2, debug, peers], #{}, Cfg),
        PeerKeys = lists:foldl(fun(A, Acc) ->
            {ok, #{pubkey := K}} = aec_peers:parse_peer_address(A),
            [K | Acc]
        end, [], PeersAddr),
        ExpectedKeys = lists:foldl(fun(N, Acc) ->
            case N of
                NodeName -> Acc;
                OtherName -> [maps:get(OtherName, NodeKeys) | Acc]
            end
        end, [], NodeNames),
        ?assertEqual(lists:sort(PeerKeys), lists:sort(ExpectedKeys))
    end, NodeNames).

check_not_forked(NodeNames, Height, Cfg) ->
    [RefBlock1 | Blocks1] = [
        request(N, [v2, 'block-by-height'], #{height => Height}, Cfg)
        || N <- NodeNames
    ],
    lists:map(fun(B) -> ?assertEqual(RefBlock1, B) end, Blocks1).