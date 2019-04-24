-module(aehttp_ga_SUITE).

%%
%% Each test assumes that the chain is at least at the height where the latest
%% consensus protocol applies hence each test reinitializing the chain should
%% take care of that at the end of the test.
%%

-include_lib("common_test/include/ct.hrl").
-include_lib("stdlib/include/assert.hrl").
-include_lib("../../aecontract/include/aecontract.hrl").
-include_lib("../../aecontract/include/hard_forks.hrl").

%% common_test exports
-export([
         all/0, groups/0, suite/0,
         init_per_suite/1, end_per_suite/1,
         init_per_group/2, end_per_group/2,
         init_per_testcase/2, end_per_testcase/2
        ]).

%% test case exports
%% external endpoints
-export([ attach/1
        , attach_fail/1
        , get_account_by_pubkey/1
        , get_account_by_pubkey_and_height/1
        , meta_fail/1
        , meta_spend/1
        ]).

-define(NODE, dev1).
-define(NODENAME, aecore_suite_utils:node_name(?NODE)).
-define(DEFAULT_GAS_PRICE, aec_test_utils:min_gas_price()).
-define(MAX_MINED_BLOCKS, 20).
-define(MINE_BLOCKS(N), aecore_suite_utils:mine_key_blocks(?NODENAME, N)).
-define(MINE_TXS(Txs), aecore_suite_utils:mine_blocks_until_txs_on_chain(?NODENAME, Txs, ?MAX_MINED_BLOCKS)).

all() ->
    [
     {group, ga_txs},
     {group, ga_info}
    ].

groups() ->
    [
     {ga_txs, [sequence],
      [ attach_fail
      , attach
      , meta_spend
      , meta_fail
      ]},

     {ga_info, [sequence],
      [ attach
      , get_account_by_pubkey
      , get_account_by_pubkey_and_height
      ]}
    ].

suite() ->
    [].

init_per_suite(Config0) ->
    Forks = aecore_suite_utils:forks(),
    DefCfg = #{<<"chain">> =>
                   #{<<"persist">> => true,
                     <<"hard_forks">> => Forks}},
    Config1 = [{symlink_name, "latest.http_ga"}, {test_module, ?MODULE}] ++ Config0,
    Config2 = aecore_suite_utils:init_per_suite([?NODE], DefCfg, Config1),
    [{nodes, [aecore_suite_utils:node_tuple(?NODE)]}]  ++ Config2.

end_per_suite(_Config) ->
    ok.

init_per_group(ga_txs, Config) -> init_for_ga(Config);
init_per_group(ga_info, Config) -> init_for_ga(Config).

init_for_ga(Config) ->
    NodeName = aecore_suite_utils:node_name(?NODE),
    aecore_suite_utils:start_node(?NODE, Config),
    aecore_suite_utils:connect(NodeName),

    ToMine = max(0, aecore_suite_utils:latest_fork_height()),
    ct:pal("ToMine ~p\n", [ToMine]),
    [ ?MINE_BLOCKS(ToMine) || ToMine > 0 ],

    %% Prepare accounts
    StartAmt = 1000 * 1000 * 1000000 * ?DEFAULT_GAS_PRICE,
    {APK, ASK, STx1} = new_account(StartAmt),
    {BPK, BSK, STx2} = new_account(StartAmt),

    {ok, _} = ?MINE_TXS([STx1, STx2]),

    %% Save account information
    Accounts = #{acc_a => #{pub_key => APK, priv_key => ASK, start_amt => StartAmt},
                 acc_b => #{pub_key => BPK, priv_key => BSK, start_amt => StartAmt}},
    [{accounts, Accounts},{node_name, NodeName} | Config].

end_per_group(_Group, Config) ->
    RpcFun = fun(M, F, A) -> rpc(?NODE, M, F, A) end,
    {ok, DbCfg} = aecore_suite_utils:get_node_db_config(RpcFun),
    aecore_suite_utils:stop_node(?NODE, Config),
    aecore_suite_utils:delete_node_db_if_persisted(DbCfg),
    ok.

init_per_testcase(_Case, Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:mock_mempool_nonce_offset(Node, 100),
    [{tc_start, os:timestamp()}|Config].

end_per_testcase(_Case, Config) ->
    [{_, Node} | _] = ?config(nodes, Config),
    aecore_suite_utils:unmock_mempool_nonce_offset(Node),
    Ts0 = ?config(tc_start, Config),
    ct:log("Events during TC: ~p", [[{N, aecore_suite_utils:all_events_since(N, Ts0)}
                                     || {_, N} <- ?config(nodes, Config)]]),
    ok.

%% ============================================================
%% Test cases
%% ============================================================

%% Attach a basic authentication contract to Account A
attach(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),

    #{tx_hash := AttachTx} = post_attach_tx(APub, APriv),

    ?MINE_TXS([AttachTx]),

    ABal1 = get_balance(APub),

    ct:pal("Cost: ~p", [ABal0 - ABal1]),
    MGP = aec_test_utils:min_gas_price(),
    ?assertEqual(ABal1, ABal0 - 1000000 * MGP - 411 * MGP),
    ok.

attach_fail(Config) ->
    #{acc_a := #{pub_key := APub, priv_key := APriv}} = proplists:get_value(accounts, Config),

    AttachTxMap = make_attach_tx_map(APub),

    Fail = fun(ATMap) ->
                Tx     = aega_test_utils:ga_attach_tx(APub, ATMap),
                STx    = aec_test_utils:sign_tx(Tx, APriv),
                SerTx  = aetx_sign:serialize_to_binary(STx),
                SendTx = aeser_api_encoder:encode(transaction, SerTx),
                {ok, 400, #{<<"reason">> := _}} = post_tx(SendTx)
           end,

    Fail(AttachTxMap#{ nonce     => 123 }),
    Fail(AttachTxMap#{ fee       => 123 }),
    Fail(AttachTxMap#{ gas_price => 123 }),

    ok.

meta_spend(Config) ->
    %% Get account information.
    #{acc_a := #{pub_key := APub, priv_key := APriv},
      acc_b := #{pub_key := BPub}} = proplists:get_value(accounts, Config),
    ABal0 = get_balance(APub),
    MGP = aec_test_utils:min_gas_price(),

    #{tx_hash := MetaTx} = post_ga_spend_tx(APub, APriv, "1", BPub, 10000, 20000 * MGP),

    ?MINE_TXS([MetaTx]),

    ABal1 = get_balance(APub),

    ct:pal("Cost1: ~p", [ABal0 - ABal1]),
    ?assertEqual(ABal1, ABal0 - (1000000 * MGP + 4744 * 1000 * MGP + 20000 * MGP + 10000)),
    ok.

meta_fail(_Config) ->
    ok.

get_account_by_pubkey(_Config) ->
    ok.

get_account_by_pubkey_and_height(_Config) ->
    ok.

%% identity_contract(Config)
%%  Create the Identity contract by account acc_c and call by accounts
%%  acc_c and acc_d. Encode create and call data in server.

%% identity_contract(Config) ->
%%     Node = proplists:get_value(node_name, Config),
%%     %% Get account information.
%%     #{acc_c := #{pub_key := CPub,
%%                  priv_key := CPriv},
%%       acc_d := #{pub_key := DPub,
%%                  priv_key := DPriv}} = proplists:get_value(accounts, Config),

%%     %% Compile test contract "identity.aes"
%%     Code = compile_test_contract("identity"),

%%     init_fun_calls(),

%%     %% Initialise contract, owned by Carl.
%%     {EncCPub, _, _} =
%%         create_compute_contract(Node, CPub, CPriv, Code, call_code("init", [])),

%%     %% Call contract main function by Carl.
%%     call_func(CPub, CPriv, EncCPub,  <<"main">>, <<"(42)">>, {<<"int">>, 42}),

%%     %% Call contract main function by Diana.
%%     call_func(DPub, DPriv, EncCPub,  <<"main">>, <<"(42)">>, {<<"int">>, 42}),

%%     force_fun_calls(Node),

%%     ok.

%% Data structure functions.
word(Val) -> #{<<"type">> => <<"word">>, <<"value">> => Val}.

tuple(Vals) -> #{<<"type">> => <<"tuple">>, <<"value">> => Vals}. %Sneaky

addr(Addr) -> <<Int:256>> = Addr, word(Int).

%% Internal access functions.

get_balance(Pubkey) ->
    Addr = aeser_api_encoder:encode(account_pubkey, Pubkey),
    {ok, 200, #{<<"balance">> := Balance}} = account_by_pubkey(Addr),
    Balance.

decode_data(Type, EncodedData) ->
    {ok, 200, #{<<"data">> := DecodedData}} =
         get_contract_decode_data(#{'sophia-type' => Type,
                                    data => EncodedData}),
    DecodedData.

%% Attach
post_attach_tx(AccPK, AccSK) ->
    AttachTxMap = make_attach_tx_map(AccPK),
    AttachTx    = aega_test_utils:ga_attach_tx(AccPK, AttachTxMap),
    sign_and_post_aetx(AccSK, AttachTx).

make_attach_tx_map(AccPK) ->
    AccId = aeser_api_encoder:encode(account_pubkey, AccPK),
    {ok, 200, #{<<"nonce">> := Nonce0}} = account_by_pubkey(AccId),
    Nonce = Nonce0 + 1,

    {ok, #{bytecode := Code, src := Src, map := #{type_info := TI}}} =
        aega_test_utils:get_contract(3, "basic_auth"),

    CallData = aega_test_utils:make_calldata(Src, "init", []),

    {ok, AuthFun} = aeb_abi:type_hash_from_function_name(<<"authorize">>, TI),

    #{ nonce => Nonce, code => Code, auth_fun => AuthFun, call_data => CallData }.

%% GA spend
post_ga_spend_tx(AccPK, AccSK, Nonce, Recipient, Amount, Fee) ->
    SpendTxMap = #{ sender_id => aeser_id:create(account, AccPK)
                  , recipient_id => aeser_id:create(account, Recipient)
                  , amount => Amount
                  , fee => Fee
                  , nonce => 0 },
    SpendTx    = aega_test_utils:spend_tx(SpendTxMap),
    Signature  = basic_auth(list_to_integer(Nonce), SpendTx, AccSK),
    AuthData   = aega_test_utils:make_calldata("basic_auth", "authorize",
                    [Nonce, aega_test_utils:to_hex_lit(64, Signature)]),
    post_ga_meta_tx(AccPK, AuthData, aetx_sign:new(SpendTx, [])).

post_ga_meta_tx(AccPK, AuthData, InnerTx) ->
    MetaTxMap = #{ auth_data => AuthData, tx => InnerTx },
    MetaTx    = aega_test_utils:ga_meta_tx(AccPK, MetaTxMap),
    SMetaTx   = aetx_sign:new(MetaTx, []),
    post_aetx(SMetaTx).

basic_auth(Nonce, Tx, Privkey) ->
    TxBin = aec_governance:add_network_id(aetx:serialize_to_binary(Tx)),
    TxHash = aec_hash:hash(tx, TxBin),
    Val = <<32:256, TxHash/binary, Nonce:256>>,
    enacl:sign_detached(aec_hash:hash(tx, Val), Privkey).

sign_and_post_aetx(PrivKey, Tx) ->
    SignedTx     = aec_test_utils:sign_tx(Tx, PrivKey),
    post_aetx(SignedTx).

post_aetx(SignedTx) ->
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    SendTx       = aeser_api_encoder:encode(transaction, SerializedTx),
    {ok, 200, #{<<"tx_hash">> := TxHash}} = post_tx(SendTx),
    #{tx_hash => TxHash, sign_tx => SignedTx}.

contract_create_compute_tx(Pubkey, Privkey, Code, InitArgument, CallerSet) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok, 200, #{<<"nonce">> := Nonce0}} = account_by_pubkey(Address),
    Nonce = Nonce0 + 1,

    CallInput =
        case InitArgument of
            B when is_binary(B) ->
                #{ arguments => InitArgument }; %% old calldata creation
            {code, CallCode} ->
                #{ call => CallCode }
        end,

    %% The default init contract.
    ContractInitEncoded0 = #{ owner_id => Address,
                              code => Code,
                              vm_version => aect_test_utils:latest_sophia_vm_version(),
                              abi_version => aect_test_utils:latest_sophia_abi_version(),
                              deposit => 2,
                              amount => 0,      %Initial balance
                              gas => 100000,   %May need a lot of gas
                              gas_price => ?DEFAULT_GAS_PRICE,
                              fee => 1400000 * ?DEFAULT_GAS_PRICE,
                              nonce => Nonce,
                              payload => <<"create contract">>},
    ContractInitEncoded1 = maps:merge(maps:merge(ContractInitEncoded0, CallInput), CallerSet),
    ContractInitEncoded = test_backwards_compatible_api(abi_optional, ContractInitEncoded1),
    sign_and_post_create_compute_tx(Privkey, ContractInitEncoded).

test_backwards_compatible_api(abi_optional, Map) ->
    case get_prn() rem 2 of
        0 -> Map;
        1 -> maps:remove(abi_version, Map)
    end;
test_backwards_compatible_api(abi_or_vm, Map) ->
    case get_prn() rem 2 of
        0 -> Map;
        1 -> maps:remove(abi_version, Map#{vm_version => maps:get(abi_version, Map)})
    end.

get_prn() ->
    case get('$prn') of
        undefined            -> put('$prn', 1), 1;
        N when is_integer(N) -> put('$prn', N+1), N+1
    end.

contract_call_compute_tx(Pubkey, Privkey, EncodedContractPubkey,
                         Function, Argument, CallerSet) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),
    %% Generate a nonce.
    {ok, 200, #{<<"nonce">> := Nonce0}} = account_by_pubkey(Address),
    Nonce = Nonce0 + 1,
    contract_call_compute_tx(Pubkey, Privkey, Nonce, EncodedContractPubkey,
                             Function, Argument, CallerSet).

contract_call_compute_tx(Pubkey, Privkey, Nonce, EncodedContractPubkey,
                         Function, Argument, CallerSet) ->
    Address = aeser_api_encoder:encode(account_pubkey, Pubkey),

    CallInput =
        case Argument of
            B when is_binary(B) ->
                #{ function => Function
                 , arguments => Argument }; %% old calldata creation
            {code, CallCode} ->
                #{ call => CallCode }
        end,

    ContractCallEncoded0 = #{ caller_id => Address,
                              contract_id => EncodedContractPubkey,
                              abi_version => aect_test_utils:latest_sophia_abi_version(),
                              amount => 0,
                              gas => 100000,    %May need a lot of gas
                              gas_price => ?DEFAULT_GAS_PRICE,
                              fee => 800000 * ?DEFAULT_GAS_PRICE,
                              nonce => Nonce,
                              payload => <<"call compute function">> },
    ContractCallEncoded1 = maps:merge(maps:merge(ContractCallEncoded0, CallInput), CallerSet),
    ContractCallEncoded = test_backwards_compatible_api(abi_or_vm, ContractCallEncoded1),
    sign_and_post_call_compute_tx(Privkey, ContractCallEncoded).

%% ============================================================
%% HTTP Requests
%% Note that some are internal and some are external!
%% ============================================================

get_micro_block_header(Hash) ->
    Host = external_address(),
    http_request(Host, get,
                 "micro-blocks/hash/"
                 ++ binary_to_list(Hash)
                 ++ "/header", []).

dry_run(Txs) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/dry-run", #{txs => Txs}).

get_key_block(Hash) ->
    Host = external_address(),
    http_request(Host, get,
                 "key-blocks/hash/"
                 ++ binary_to_list(Hash), []).

get_key_blocks_current_height() ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/current/height", []).

get_key_block_at_height(Height) ->
    Host = external_address(),
    http_request(Host, get, "key-blocks/height/" ++ integer_to_list(Height), []).

get_contract_bytecode(SourceCode) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/code/compile",
                 #{ <<"code">> => SourceCode, <<"options">> => <<>> }).

%% get_contract_create(Data) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "debug/contracts/create", Data).

get_contract_create_compute(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/create/compute", Data).

%% get_contract_call(Data) ->
%%     Host = internal_address(),
%%     http_request(Host, post, "debug/contracts/call", Data).

get_contract_call_compute(Data) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/call/compute", Data).

get_contract_decode_data(Request) ->
    Host = internal_address(),
    http_request(Host, post, "debug/contracts/code/decode-data", Request).

get_contract_call_object(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/"++binary_to_list(TxHash)++"/info", []).

get_tx(TxHash) ->
    Host = external_address(),
    http_request(Host, get, "transactions/" ++ binary_to_list(TxHash), []).

create_spend_tx(RecipientId, Amount, Fee) ->
    Sender = maps:get(pubkey, aecore_suite_utils:patron()),
    SenderId = aeser_api_encoder:encode(account_pubkey, Sender),
    create_spend_tx(SenderId, RecipientId, Amount, Fee, <<"post spend tx">>).

create_spend_tx(SenderId, RecipientId, Amount, Fee, Payload) ->
    Host = internal_address(),
    http_request(Host, post, "debug/transactions/spend",
                 #{sender_id => SenderId,
                   recipient_id => RecipientId,
                   amount => Amount,
                   fee => Fee,
                   payload => Payload}).

account_by_pubkey(Id) ->
    Host = external_address(),
    http_request(Host, get, "accounts/" ++ http_uri:encode(Id), []).

post_tx(TxSerialized) ->
    Host = external_address(),
    http_request(Host, post, "transactions", #{tx => TxSerialized}).

sign_tx(Tx) ->
    {ok, TxSer} = aeser_api_encoder:safe_decode(transaction, Tx),
    UTx = aetx:deserialize_from_binary(TxSer),
    STx = aec_test_utils:sign_tx(UTx, [maps:get(privkey, aecore_suite_utils:patron())]),
    aeser_api_encoder:encode(transaction, aetx_sign:serialize_to_binary(STx)).

%% ============================================================
%% private functions
%% ============================================================
rpc(Mod, Fun, Args) ->
    rpc(?NODE, Mod, Fun, Args).

rpc(Node, Mod, Fun, Args) ->
    rpc:call(aecore_suite_utils:node_name(Node), Mod, Fun, Args, 5000).

external_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"external">>, <<"port">>],
                aehttp, [external, port], 8043]),
    "http://127.0.0.1:" ++ integer_to_list(Port).     % good enough for requests

internal_address() ->
    Port = rpc(aeu_env, user_config_or_env,
              [ [<<"http">>, <<"internal">>, <<"port">>],
                aehttp, [internal, port], 8143]),
    "http://127.0.0.1:" ++ integer_to_list(Port).

http_request(Host, get, Path, Params) ->
    URL = binary_to_list(
            iolist_to_binary([Host, "/v2/", Path, encode_get_params(Params)])),
    ct:log("GET ~p", [URL]),
    R = httpc_request(get, {URL, []}, [], []),
    process_http_return(R);
http_request(Host, post, Path, Params) ->
    URL = binary_to_list(iolist_to_binary([Host, "/v2/", Path])),
    {Type, Body} = case Params of
                       Map when is_map(Map) ->
                           %% JSON-encoded
                           {"application/json", jsx:encode(Params)};
                       [] ->
                           {"application/x-www-form-urlencoded",
                            http_uri:encode(Path)}
                   end,
    %% lager:debug("Type = ~p; Body = ~p", [Type, Body]),
    ct:log("POST ~p, type ~p, Body ~p", [URL, Type, Body]),
    R = httpc_request(post, {URL, [], Type, Body}, [], []),
    process_http_return(R).

httpc_request(Method, Request, HTTPOptions, Options) ->
    httpc_request(Method, Request, HTTPOptions, Options, test_browser).

httpc_request(Method, Request, HTTPOptions, Options, Profile) ->
    {ok, Pid} = inets:start(httpc, [{profile, Profile}], stand_alone),
    Response = httpc:request(Method, Request, HTTPOptions, Options, Pid),
    ok = gen_server:stop(Pid, normal, infinity),
    Response.

encode_get_params(#{} = Ps) ->
    encode_get_params(maps:to_list(Ps));
encode_get_params([{K, V}|T]) ->
    ["?", [str(K),"=",uenc(V)
           | [["&", str(K1), "=", uenc(V1)]
              || {K1, V1} <- T]]];
encode_get_params([]) ->
    [].

str(A) when is_atom(A) ->
    str(atom_to_binary(A, utf8));
str(S) when is_list(S); is_binary(S) ->
    S.

uenc(I) when is_integer(I) ->
    uenc(integer_to_list(I));
uenc(V) ->
    http_uri:encode(V).

process_http_return(R) ->
    case R of
        {ok, {{_, ReturnCode, _State}, _Head, Body}} ->
            try
                ct:log("Return code ~p, Body ~p", [ReturnCode, Body]),
                Result = case iolist_to_binary(Body) of
                             <<>> -> #{};
                             BodyB ->
                                 jsx:decode(BodyB, [return_maps])
                         end,
                {ok, ReturnCode, Result}
            catch
                error:E ->
                    {error, {parse_error, [E, erlang:get_stacktrace()]}}
            end;
        {error, _} = Error ->
            Error
    end.

new_account(Balance) ->
    {Pubkey, Privkey} = generate_key_pair(),
    Fee = 20000 * ?DEFAULT_GAS_PRICE,
    {ok, 200, #{<<"tx">> := SpendTx}} =
        create_spend_tx(aeser_api_encoder:encode(account_pubkey, Pubkey), Balance, Fee),
    SignedSpendTx = sign_tx(SpendTx),
    {ok, 200, #{<<"tx_hash">> := SpendTxHash}} = post_tx(SignedSpendTx),
    {Pubkey, Privkey, SpendTxHash}.

sign_and_post_create_compute_tx(Privkey, CreateEncoded) ->
    {ok, 200, #{<<"tx">> := EncodedUnsignedTx,
              <<"contract_id">> := EncodedPubkey}} =
        get_contract_create_compute(CreateEncoded),
    {ok, DecodedPubkey} = aeser_api_encoder:safe_decode(contract_pubkey,
                                                 EncodedPubkey),
    Tx = sign_and_post_tx(Privkey, EncodedUnsignedTx),
    {Tx, EncodedPubkey, DecodedPubkey}.

sign_and_post_call_compute_tx(Privkey, CallEncoded) ->
    {ok, 200, #{<<"tx">> := EncodedUnsignedTx}} =
        get_contract_call_compute(CallEncoded),
    sign_and_post_tx(Privkey, EncodedUnsignedTx).

sign_and_post_tx(PrivKey, EncodedUnsignedTx) ->
    {ok, SerializedUnsignedTx} = aeser_api_encoder:safe_decode(transaction,
                                                        EncodedUnsignedTx),
    UnsignedTx = aetx:deserialize_from_binary(SerializedUnsignedTx),
    SignedTx = aec_test_utils:sign_tx(UnsignedTx, PrivKey),
    SerializedTx = aetx_sign:serialize_to_binary(SignedTx),
    SendTx = aeser_api_encoder:encode(transaction, SerializedTx),
    {ok, 200, #{<<"tx_hash">> := TxHash}} = post_tx(SendTx),
    #{tx_hash => TxHash, tx_encoded => EncodedUnsignedTx}.

tx_in_chain(TxHash) ->
    case get_tx(TxHash) of
        {ok, 200, #{<<"block_hash">> := <<"none">>}} ->
            ct:log("Tx not mined, but in mempool"),
            false;
        {ok, 200, #{<<"block_hash">> := _}} -> true;
        {ok, 404, _} -> false
    end.

wait_for_tx_hash_on_chain(Node, TxHash) ->
    case tx_in_chain(TxHash) of
        true -> ok;
        false ->
            case aecore_suite_utils:mine_blocks_until_txs_on_chain(Node, [TxHash], ?MAX_MINED_BLOCKS) of
                {ok, _Blocks} -> ok;
                {error, _Reason} -> did_not_mine
            end
    end.

%% make_params(L) ->
%%     make_params(L, []).

%% make_params([], Accum) ->
%%     maps:from_list(Accum);
%% make_params([H | T], Accum) when is_map(H) ->
%%     make_params(T, maps:to_list(H) ++ Accum);
%% make_params([{K, V} | T], Accum) ->
%%     make_params(T, [{K, V} | Accum]).

generate_key_pair() ->
    #{ public := Pubkey, secret := Privkey } = enacl:sign_keypair(),
    {Pubkey, Privkey}.

call_code(Fun, Args) ->
    Type    = ["(", string:join([ type_of_arg(Arg) || Arg <- Args ], ", "), ")"],
    BinArgs = args_to_list(Args),
    {code, list_to_binary(
        [ "contract Call =\n"
        , "  function ", Fun, " : ", Type, " => _\n"
        , "  function __call() = ", Fun, "(", BinArgs, ")\n" ])}.

type_of_arg(N) when is_integer(N) -> "int";
type_of_arg({string, _}) -> "string";
type_of_arg([H | _]) -> ["list(", type_of_arg(H), ")"];
type_of_arg([]) -> "list(int)"; %% Don't know the element type
type_of_arg(B) when is_binary(B), byte_size(B) == 32 -> "address";
type_of_arg(B) when is_binary(B), byte_size(B) == 64 -> "signature";
type_of_arg(T) when is_tuple(T) ->
    ["(", string:join([ type_of_arg(X) || X <- tuple_to_list(T) ], ","), ")"];
type_of_arg(M) when is_map(M) ->
    case maps:to_list(M) of
        []  -> %% empty map: can't infer type, default to int/int
            "map(int, int)";
        [{K, V} | _] ->
            ["map(", type_of_arg(K), ", ", type_of_arg(V), ")"]
    end.

%% args_to_binary(Args) -> binary_string().
%%  Take a list of arguments in "erlang format" and generate an
%%  argument binary string. Strings are handled naively now.

args_to_binary(Args) ->
    %% ct:pal("Args ~tp\n", [Args]),
    BinArgs = list_to_binary([$(,args_to_list(Args),$)]),
    %% ct:pal("BinArgs ~tp\n", [BinArgs]),
    BinArgs.

args_to_list([A]) -> [arg_to_list(A)];          %The last one
args_to_list([A1|Rest]) ->
    [arg_to_list(A1),$,|args_to_list(Rest)];
args_to_list([]) -> [].

%%arg_to_list(<<N:256>>) -> integer_to_list(N);
arg_to_list(N) when is_integer(N) -> integer_to_list(N);
arg_to_list(B) when is_binary(B) ->             %A key
    <<"0x", Enc/binary>> = aeu_hex:hexstring_encode(B),
    ["#", binary_to_list(Enc)];
arg_to_list({string, S}) -> ["\"",S, "\""];
arg_to_list(L) when is_list(L) ->
    [$[,args_to_list(L),$]];
arg_to_list(T) when is_tuple(T) ->
    [$(,args_to_list(tuple_to_list(T)),$)];
arg_to_list(M) when is_map(M) ->
    [${,map_to_list(maps:to_list(M)),$}].

map_to_list([{K, V}]) -> [$[,arg_to_list(K),"] = ",arg_to_list(V)];
map_to_list([{K, V},Fields]) ->
    [$[,arg_to_list(K),"] = ",arg_to_list(V),$,|map_to_list(Fields)];
map_to_list([]) -> [].
