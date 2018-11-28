-module(aest_mining_pool_SUITE).

% Common Test exports
-export([all/0]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

% Tests
-export([test_pool_manager/1]).

%--- Common Test exports -------------------------------------------------------

all() ->
    [
        test_pool_manager
    ].

init_per_testcase(_TC, Cfg) ->
    aest_nodes:ct_setup(Cfg).

end_per_testcase(_TC, Cfg) ->
    aest_nodes:ct_cleanup(Cfg).

%--- Tests ---------------------------------------------------------------------

test_pool_manager(Cfg) ->
    NodeSpecs = aest_nodes:cluster([n1, n2, n3], #{mining => #{autostart => true}}),
    Node = aest_nodes:spec(n4, [n1, n2, n3], #{mining => #{autostart => false}}),
    aest_nodes:setup_nodes(NodeSpecs ++ [Node], Cfg),

    Nodes = [n1, n2, n3, n4],
    [aest_nodes:start_node(N, Cfg) || N <- Nodes],
    aest_nodes:wait_for_startup(Nodes, 0, Cfg),
    PendingKeyBlock = aest_nodes:get_pending_key_block(n4),
    ct:log("~p", [PendingKeyBlock]),

    % TODO: Post mined block here
    aest_nodes:post_key_block(n4, PendingKeyBlock),
    ok.
