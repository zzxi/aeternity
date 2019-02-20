-module(aest_db_SUITE).

%=== EXPORTS ===================================================================

% Common Test exports
-export([all/0]).
-export([init_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).
-export([end_per_suite/1]).

% Test cases
-export([
    node_can_reuse_db_of_other_node/1,
    roma_node_can_reuse_db_of_other_roma_node/1,
    node_can_reuse_db_of_roma_node/1,
    node_can_reuse_db_of_roma_node_with_contract_txs/1
]).

%=== INCLUDES ==================================================================

-include_lib("stdlib/include/assert.hrl").

%=== MACROS ====================================================================

-define(STARTUP_TIMEOUT, 20000).
-define(MINING_TIMEOUT,   3000).
-define(GRACEFUL_STOP_TIMEOUT, 60000).

%=== COMMON TEST FUNCTIONS =====================================================

all() -> [
    node_can_reuse_db_of_other_node,
    roma_node_can_reuse_db_of_other_roma_node,
    node_can_reuse_db_of_roma_node,
    node_can_reuse_db_of_roma_node_with_contract_txs
].

init_per_suite(Config) ->
    Config.

init_per_testcase(_TC, Config) ->
    aest_nodes:ct_setup(Config).

end_per_testcase(_TC, Config) ->
    aest_nodes:ct_cleanup(Config).

end_per_suite(_Config) -> ok.

%=== TEST CASES ================================================================

node_can_reuse_db_of_other_node(Cfg) ->
    node_can_reuse_db_of_other_node_(fun node_spec/2, Cfg).

roma_node_can_reuse_db_of_other_roma_node(Cfg) ->
    node_can_reuse_db_of_other_node_(fun roma_node_spec/2, Cfg).

node_can_reuse_db_of_roma_node(Cfg) ->
    node_can_reuse_db_of_other_node_(fun roma_node_spec/2, fun node_spec/2, Cfg).

node_can_reuse_db_of_roma_node_with_contract_txs(Cfg) ->
    node_can_reuse_db_of_other_node_(fun roma_node_spec/2, fun node_spec/2, {fun populate_db_with_contract_txs/2, fun assert_db_with_contract_txs_reused/3}, Cfg).

%=== INTERNAL FUNCTIONS ========================================================

node_can_reuse_db_of_other_node_(NodeSpecFun, Cfg) ->
    node_can_reuse_db_of_other_node_(NodeSpecFun, NodeSpecFun, Cfg).

node_can_reuse_db_of_other_node_(CreateDbNodeSpecFun, ReuseDbNodeSpecFun, Cfg) ->
    node_can_reuse_db_of_other_node_(CreateDbNodeSpecFun, ReuseDbNodeSpecFun, {fun populate_db/2, fun assert_db_reused/3}, Cfg).

node_can_reuse_db_of_other_node_(CreateDbNodeSpecFun, ReuseDbNodeSpecFun, {PopulateDbFun, AssertDbFun}, Cfg) ->
    DbHostPath = node_db_host_path(node1, Cfg),
    N1 = CreateDbNodeSpecFun(node1, DbHostPath),
    N2 = ReuseDbNodeSpecFun(node2, DbHostPath),
    aest_nodes:setup_nodes([N1, N2], Cfg),
    start_and_wait_node(node1, ?STARTUP_TIMEOUT, Cfg),
    DbFingerprint = PopulateDbFun(node1, Cfg),
    aest_nodes:stop_node(node1, ?GRACEFUL_STOP_TIMEOUT, Cfg),
    start_and_wait_node(node2, ?STARTUP_TIMEOUT, Cfg),
    AssertDbFun(node2, DbFingerprint, Cfg),
    ok.

populate_db(NodeName, Cfg) ->
    TargetHeight = 3,
    aest_nodes:wait_for_value({height, TargetHeight}, [NodeName], TargetHeight * ?MINING_TIMEOUT, Cfg),
    #{hash := BlockHash} = aest_nodes:get_block(node1, TargetHeight),
    _DbFingerprint = {TargetHeight, BlockHash}.

assert_db_reused(NodeName, {TargetHeight, BlockHash} = _DbFingerprint, Cfg) ->
    aest_nodes:wait_for_value({height, TargetHeight}, [NodeName], ?STARTUP_TIMEOUT, Cfg),
    ?assertMatch({ok, 200, _}, get_block_by_hash(NodeName, BlockHash)),
    ok.

a_fresh_keypair() ->
    enacl:sign_keypair().

a_fresh_contract_id() ->
    #{ public := PubKey, secret := _ } = a_fresh_keypair(),
    aec_id:create(contract, PubKey).

populate_db_with_contract_txs(NodeName, Cfg) ->
    #{ public := PubKey, secret := PrivKey } = a_fresh_keypair(),
    CallTxArgs = #{ caller_id   => aec_id:create(account, PubKey)
        , nonce       => 1
        , contract_id => a_fresh_contract_id()
        , fee         => 500000 * aest_nodes:gas_price()
        , ttl         => 100
        , abi_version => 1
        , amount      => 0
        , gas         => 100
        , gas_price   => 123456
        , call_data   => <<"CALL DATA">>
    },
    foo = aest_nodes:post_contract_call_tx(NodeName, PrivKey, CallTxArgs),
    _DbFingerprint = exit(not_yet_implemented).

assert_db_with_contract_txs_reused(NodeName, _DbFingerprint, Cfg) ->
    exit(not_yet_implemented).

get_block_by_hash(NodeName, Hash) ->
    aest_nodes:request(NodeName, 'GetKeyBlockByHash', #{hash => Hash}).

start_and_wait_node(NodeName, Timeout, Cfg) ->
    aest_nodes:start_node(NodeName, Cfg),
    aest_nodes:wait_for_value({height, 0}, [NodeName], Timeout, Cfg),
    %% Hardcode expectation that node picks user config
    #{network_id := <<"ae_system_test">>} = aest_nodes:get_status(NodeName),
    ok.

node_db_host_path(NodeName, Config) ->
    {priv_dir, PrivDir} = proplists:lookup(priv_dir, Config),
    filename:join(PrivDir, format("~s_db", [NodeName])).

format(Fmt, Args) ->
    iolist_to_binary(io_lib:format(Fmt, Args)).

node_spec(Name, DbHostPath) ->
    node_spec(Name, DbHostPath, true).
node_spec(Name, DbHostPath, Mining) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:local"}, db_path => {DbHostPath, DbGuestPath}, mining => #{autostart => Mining}}).

%% Last Roma release.
roma_node_spec(Name, DbHostPath) ->
    roma_node_spec(Name, DbHostPath, true).
roma_node_spec(Name, DbHostPath, Mining) ->
    DbGuestPath = "/home/aeternity/node/data/mnesia",
    aest_nodes:spec(Name, [], #{source  => {pull, "aeternity/aeternity:v1.4.0"}, db_path => {DbHostPath, DbGuestPath}, config_guest_path => "/home/aeternity/.epoch/epoch/epoch.yaml", mining => #{autostart => Mining}}).
