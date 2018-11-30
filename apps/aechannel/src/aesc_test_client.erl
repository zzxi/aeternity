-module(aesc_test_client).

-export([start_link/1]).
-export([local_test/0]).
-export([key_pair/2,
         initiator_start/1,
         initiator_start/4,
         responder_start/1,
         responder_start/4
        ]).

-record(kpair, {pub_key  :: aec_keys:pubkey(),
                priv_key :: aec_keys:privkey()}).

-record(state, {role :: initiator | responder,
                keys :: #kpair{},
                fsm  :: pid(),
                contract_id :: aect_contracts:pubkey(),
                status = unopened :: unopened
                                   | opened
                                   | contract_created,
                test_start_time = not_set :: not_set | non_neg_integer(),
                calls_left = 100000 :: non_neg_integer()
               }).


%% gen_server callbacks
-export([ init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3]).

-include_lib("apps/aecontract/src/aecontract.hrl").

-define(INITIATOR_AMT, 50000).
-define(RESPONDER_AMT, 50000).
-define(PUSH_AMT, 2).
-define(CHANNEL_RESERVE, 10).

%% INITIATOR
%% ak_ozzwBYeatmuN818LjDDDwRSiBSvrqt4WU7WvbGsZGVre72LTS
-define(IPUB_KEY, <<106,184,29,213,77,73,184,77,59,65,33,156,241,78,239,173,39,2,126,254,111,28,73,150,6,150,66,20,47,81,213,154>>).
-define(IPRIV_KEY, <<133,143,10,3,177,135,2,205,204,153,181,19,83,137,93,186,100,92,12,201,228,174,194,70,27,220,3,227,212,32,203,247,106,184,29,213,77,73,184,77,59,65,33,156,241,78,239,173,39,2,126,254,111,28,73,150,6,150,66,20,47,81,213,154>>).

%% RESPONDER
%% ak_26xYuZJnxpjuBqkvXQ4EKb4Ludt8w3rGWREvEwm68qdtJLyLwq
-define(RPUB_KEY, <<145,57,82,197,159,203,87,93,38,245,163,158,237,249,101,141,158,185,198,87,190,11,15,96,80,225,138,111,252,37,59,79>>).
-define(RPRIV_KEY, <<55,112,8,133,136,166,103,209,225,173,157,98,179,248,227,75,64,253,175,97,81,149,27,108,35,160,80,16,121,176,159,138,145,57,82,197,159,203,87,93,38,245,163,158,237,249,101,141,158,185,198,87,190,11,15,96,80,225,138,111,252,37,59,79>>).


key_pair(PrivKey, PubKey) ->
    #kpair{pub_key  = PubKey,
           priv_key = PrivKey}.

local_test() ->
    Init = key_pair(?IPRIV_KEY, ?IPUB_KEY),
    Resp = key_pair(?RPRIV_KEY, ?RPUB_KEY),
    Initiator = Init#kpair.pub_key,
    Responder = Resp#kpair.pub_key,
    Address = {"localhost", 4444},
    responder_start(Address, Initiator, Responder, Resp),
    initiator_start(Address, Initiator, Responder, Init).
    
initiator_start(Address) ->
    Init = key_pair(?IPRIV_KEY, ?IPUB_KEY),
    initiator_start(Address, ?IPUB_KEY, ?RPUB_KEY, Init).

initiator_start({ResponderHost, ResponderPort},
                 InitiatorId, ResponderId,
                Keys) ->
    start_link(#{role      => initiator,
                 keys      => Keys,
                 initiator => InitiatorId,
                 responder => ResponderId,
                 host      => ResponderHost,
                 port      => ResponderPort}).

responder_start(Address) ->
    Resp = key_pair(?RPRIV_KEY, ?RPUB_KEY),
    responder_start(Address, ?IPUB_KEY, ?RPUB_KEY, Resp).

responder_start({ResponderHost, ResponderPort},
                 InitiatorId, ResponderId,
                Keys) ->
    start_link(#{role      => responder,
                 keys      => Keys,
                 initiator => InitiatorId,
                 responder => ResponderId,
                 host      => ResponderHost,
                 port      => ResponderPort}).


%%%===================================================================
%%% GenServer API
%%%===================================================================

start_link(#{} = Arg) ->
    gen_server:start_link(?MODULE, Arg, []).

init(#{role      := Role,
       keys      := Keys,
       initiator := InitiatorId,
       responder := ResponderId,
       host      := ResponderHost,
       port      := ResponderPort
      }) ->
    {ok, Fsm} =
        case Role of
            initiator ->
                aesc_fsm:initiate(ResponderHost,
                                  ResponderPort,
                                  connect_opts(initiator,
                                              InitiatorId,
                                              ResponderId));
            responder ->
                aesc_fsm:respond(ResponderPort,
                                 connect_opts(responder,
                                              InitiatorId,
                                              ResponderId))
        end,
    log_start_msg(InitiatorId, ResponderId, Role),
    {ok, #state{role = Role,
                keys = Keys,
                fsm  = Fsm}}.

handle_call(Call, _From, #state{role = Role} = State) ->
    lager:info("Unhandled ~p call ~p", [Role, Call]),
    {reply, _Reply = ok12, State}.

handle_info({aesc_fsm, Fsm, Msg}, #state{fsm = Fsm} = State) ->
    process(Msg, State),
    State1 = next_state(Msg, State),
    {noreply, State1};
handle_info(create_transfer, State) ->
    create_transfer(State),
    {noreply, State};
handle_info(create_contract, State) ->
    create_contract(State),
    {noreply, State};
handle_info(call_contract, #state{calls_left = Left,
                                  fsm        = Fsm,
                                  test_start_time = Tmst,
                                  role       = Role} = State) when Left < 1 ->
    aesc_fsm:shutdown(Fsm),
    TimeL = aeu_time:now_in_msecs() - Tmst,
    log_("is done,", "closing with a mutual agreement", Role),
    log_("took", integer_to_list(TimeL) ++ " ms", Role),
    {noreply, State};
handle_info(call_contract, State) ->
    call_contract(State),
    State1 = set_time_if_needed(State),
    {noreply, State1};
handle_info(Info, #state{role = Role} = State) ->
    lager:info("Unhandled ~p info ~p", [Role, Info]),
    {noreply, State}.

handle_cast(Cast, #state{role = Role} = State) ->
    lager:info("Unhandled ~p cast ~p", [Role, Cast]),
    {noreply, State}.

terminate(_Reason, #state{fsm = Fsm, role = Role}) ->
    log_("is done,", "dying", Role),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

connect_opts(Role, InitiatorId, ResponderId) ->
    #{initiator        => InitiatorId,
      responder        => ResponderId,
      role             => Role,
      lock_period      => 3,
      minimum_depth    => 0,
      push_amount      => ?PUSH_AMT,
      initiator_amount => ?INITIATOR_AMT,
      responder_amount => ?RESPONDER_AMT,
      channel_reserve  => ?CHANNEL_RESERVE,
      noise            => [{noise, <<"Noise_NN_25519_ChaChaPoly_BLAKE2b">>}]
     }.

log_start_msg(I, R, Role) ->
    lager:info("Starting ~p for channel between ~p and ~p",
               [Role,
                aehttp_api_encoder:encode(account_pubkey, I),
                aehttp_api_encoder:encode(account_pubkey, R)]),
    ok.

process(#{type := report, tag := Tag, info := Msg0}, #state{role=Role}) ->
    {Action, Msg} =
        case Tag of
            info when Msg0 =:= {died,normal} -> {"will now", "die"};
            info         -> {"received an info", Msg0};
            on_chain_tx  ->
                {TxType, _Tx} = aetx:specialize_type(aetx_sign:tx(Msg0)),
                {"informed about on-chain transaction", TxType};
            update       ->
                {TxType, _Tx} = aetx:specialize_type(aetx_sign:tx(Msg0)),
                {"received a co-signed", TxType}
        end,
    pass;
    %log_(Action, Msg, Role);
process(#{type := sign, tag := Type, info := Tx}, #state{fsm=Fsm,
                                                         keys=Keys,
                                                         role=Role}) ->
    %log_("asked to sign", Type, Role),
    SignedTx = sign_tx(Tx, Keys#kpair.priv_key),
    aesc_fsm:signing_response(Fsm, Type, SignedTx);
process(Msg, #state{role=Role}) ->
    log_("UNHANDLED PROCESS", Msg, Role).

next_state(#{type := report, tag := info, info := open},
            #state{role=Role} = State) ->
    log_("is", "opened now!", Role),
    case Role of
        initiator ->
            timer:send_after(1000, create_contract);
        responder ->
            pass
            %log_("is waiting for other participant to create",
            %     "a contract", Role)
    end,
    State#state{status = opened};
next_state(#{type := report, tag := update, info := SignedTx},
            #state{role=Role, status=opened} = State) ->
    case aetx:specialize_type(aetx_sign:tx(SignedTx)) of
        {channel_offchain_tx, Tx} ->
            [Update] = aesc_offchain_tx:updates(Tx),
            true = aesc_offchain_update:is_contract_create(Update),
            log_("has a", "contract", Role),
            case Role of
                initiator ->
                    timer:send_after(0, call_contract);
                responder ->
                    pass
                    %log_("is waiting for other participant to call",
                    %    "a contract", Role)
            end,
            Round = aesc_offchain_tx:round(Tx),
            Owner = aesc_offchain_update:extract_caller(Update),
            ContractPubkey =
                aect_contracts:compute_contract_pubkey(Owner, Round),
            State#state{status      = contract_created,
                        contract_id = ContractPubkey
                       };
        {channel_create_tx, _} ->
            State
    end;
next_state(#{type := report, tag := update, info := _SignedTx},
            #state{role=Role, status=contract_created,
                   calls_left=Left} = State) ->

    %log_("is waiting for other participant to call a contract",
    %     integer_to_list(Left), Role),
    case Role of
        initiator ->
            timer:send_after(0, call_contract);
        responder ->
            pass
            %log_("is waiting for other participant to call",
            %    "a contract", Role)
    end,
    UpdatedLeft = Left - 1,
    case UpdatedLeft rem 200 =:= 0 of
        true ->
            log_("has " ++ integer_to_list(UpdatedLeft), "contract calls remaining", Role);
        false -> pass
    end,
    State#state{calls_left = Left - 1};
next_state(_, State) -> State.

log_(Action, Msg, Role) ->
    lager:info("~p ~s ~s", [Role, Action, Msg]).

-spec sign_tx(aetx:tx(), list(binary()) | binary()) -> aetx_sign:signed_tx().
sign_tx(Tx, PrivKey) when is_binary(PrivKey) ->
    sign_tx(Tx, [PrivKey]);
sign_tx(Tx, PrivKeys) when is_list(PrivKeys) ->
    Bin = aetx:serialize_to_binary(Tx),
    BinForNetwork = aec_governance:add_network_id(Bin),
    Signatures = [ enacl:sign_detached(BinForNetwork, PrivKey) || PrivKey <- PrivKeys ],
    aetx_sign:new(Tx, Signatures).

create_transfer(#state{fsm = Fsm, role = Role}) ->
    log_("is creating", "a transfer", Role),
    ok = aesc_fsm:upd_transfer(Fsm,
                               ?IPUB_KEY,
                               ?RPUB_KEY,
                               1),
    ok.

create_contract(#state{fsm = Fsm, role = Role,
                     calls_left = Left}) ->
    %log_("is !!!!!!!!!!!!!!!!!! creating", "a contract", Role),
    TestName = "counter",
    BinCode = compile_contract(TestName),
    CallData = make_calldata_from_code(BinCode, init, {42}), 
    log_("is CREATING contract", integer_to_list(Left), Role),
    ok = aesc_fsm:upd_create_contract(Fsm,
                                      #{vm_version => ?AEVM_01_Sophia_01,
                                        deposit    => 5,
                                        code       => BinCode,
                                        call_data  => CallData}),
    ok.

call_contract(#state{fsm = Fsm, role = Role, contract_id = ContractPubkey,
                     calls_left = Left}) ->
    %log_("is calling a contract", integer_to_list(Left), Role),
    TestName = "counter",
    BinCode = compile_contract(TestName),
    CallData = make_calldata_from_code(BinCode, tick, {}), 
    ok = aesc_fsm:upd_call_contract(Fsm,
                                    #{vm_version => ?AEVM_01_Sophia_01,
                                      amount     => 0,
                                      contract   => ContractPubkey,
                                      call_data  => CallData}),
    ok.

make_calldata_raw(<<FunHashInt:256>>, Args0) ->
    Args = translate_pubkeys(if is_tuple(Args0) -> Args0; true -> {Args0} end),
    aeso_data:to_binary({FunHashInt, Args}).

make_calldata_from_code(Code, Fun, Args) when is_atom(Fun) ->
    make_calldata_from_code(Code, atom_to_binary(Fun, latin1), Args);
make_calldata_from_code(Code, Fun, Args) when is_binary(Fun) ->
    #{type_info := TypeInfo} = aeso_compiler:deserialize(Code),
    case aeso_abi:type_hash_from_function_name(Fun, TypeInfo) of
        {ok, TypeHash} -> make_calldata_raw(TypeHash, Args);
        {error, _} = Err -> error({bad_function, Fun, Err})
    end.

translate_pubkeys(<<N:256>>) -> N;
translate_pubkeys([H|T]) ->
  [translate_pubkeys(H) | translate_pubkeys(T)];
translate_pubkeys(T) when is_tuple(T) ->
  list_to_tuple(translate_pubkeys(tuple_to_list(T)));
translate_pubkeys(M) when is_map(M) ->
  maps:from_list(translate_pubkeys(maps:to_list(M)));
translate_pubkeys(X) -> X.

-spec compile_contract(string() | atom()) -> string().
compile_contract(Name) ->
    File = Name ++ ".aes",
    FileName = filename:join(contract_path(), File),
    {ok, ContractBin} = file:read_file(FileName),
    Contract = binary_to_list(ContractBin),
    aeso_compiler:from_string(Contract, []). % [pp_icode, pp_assembler, pp_bytecode]).

contract_path() ->
    {ok, Cwd} = file:get_cwd(),
    N   = length(filename:split(Cwd)),
    Rel = ["apps", "aesophia", "test", "contracts"],
    %% Try the first matching directory (../)*Rel
    Cand = fun(I) -> filename:join(lists:duplicate(I, "..") ++ Rel) end,
    case [ Dir || Dir <- lists:map(Cand, lists:seq(0, N)), filelib:is_dir(Dir) ] of
        [Dir | _] -> Dir;
        []        -> error(failed_to_find_contract_dir)
    end.

set_time_if_needed(#state{test_start_time = not_set} = State) ->
   State#state{test_start_time = aeu_time:now_in_msecs()}; 
set_time_if_needed(#state{} = State) -> State.
