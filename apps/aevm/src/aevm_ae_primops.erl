%%%-------------------------------------------------------------------
%%% @copyright (C) 2018, Aeternity Anstalt
%%% @doc
%%%     Handle interaction with the aeternity chain
%%%     through calls to AEternity primitive operations at address 0.
%%% @end
%%% Created : 22 May 2018
%%%-------------------------------------------------------------------

-module(aevm_ae_primops).
-export([call/3, is_local_primop/1]).

-include_lib("aebytecode/include/aeb_opcodes.hrl").
-include("aevm_ae_primops.hrl").

-record(chain, {api, state}).

-ifdef(COMMON_TEST).
-define(TEST_LOG(Format, Data),
        try ct:log(Format, Data)
        catch
            %% Enable setting up node with "test" rebar profile.
            error:undef -> ok
        end).
-else.
-define(TEST_LOG(Format, Data), ok).
-endif.

-spec call(Value::non_neg_integer(), Data::binary(), StateIn) ->
                  {ok, ReturnValue, GasSpent::non_neg_integer(), StateOut} |
                  {error, Reason} when
      StateIn :: State,
      StateOut :: State,
      State :: aevm_eeevm_state:state(),
      ReturnValue :: {ok, binary()} | {error, any()},
      Reason :: ?AEVM_PRIMOP_ERR_REASON_OOG(OogResource, OogCost, State)
              | any(),
      OogResource :: any(),
      OogCost :: pos_integer().
call(Value, Data, StateIn) ->
    case call_primop(get_primop(Data), Value, Data, StateIn) of
        {ok, _, _, _} = Ok -> Ok;
        {error, _} = Err ->
            ?TEST_LOG("Primop call error ~p~n~p:~p(~p, ~p, State)",
                      [Err, ?MODULE, ?FUNCTION_NAME, Value, Data]),
            Err
    end.

call_primop(PrimOp, Value, Data, StateIn) ->
    case PrimOp of
        PrimOp when ?PRIM_CALL_IN_MAP_RANGE(PrimOp) ->  %% Map primops need the full VM state
            try map_call(PrimOp, Value, Data, StateIn)
            catch _:Err -> {error, {Err, erlang:get_stacktrace()}}
            end;
        _ ->
            ChainIn = #chain{api   = aevm_eeevm_state:chain_api(StateIn),
                             state = aevm_eeevm_state:chain_state(StateIn)},
            case call_(PrimOp, Value, Data, ChainIn) of
                {ok, ReturnValue, GasSpent, ChainStateOut} ->
                    StateOut = aevm_eeevm_state:set_chain_state(ChainStateOut, StateIn),
                    {ok, ReturnValue, GasSpent, StateOut};
                {error, _} = Err -> Err
            end
    end.

is_local_primop(Data) ->
    case get_primop(Data) of
        Op when ?PRIM_CALL_IN_MAP_RANGE(Op) -> true;
        _ -> false
    end.

call_(PrimOp, Value, Data, State) ->
    try
        case PrimOp of
            ?PRIM_CALL_SPEND ->
                spend_call(Value, Data, State);
            PrimOp when ?PRIM_CALL_IN_ORACLE_RANGE(PrimOp) ->
                oracle_call(PrimOp, Value, Data, State);
            PrimOp when ?PRIM_CALL_IN_AENS_RANGE(PrimOp) ->
                aens_call(PrimOp, Value, Data, State)
        end
    catch _T:_Err ->
            ?TEST_LOG("Primop illegal call ~p:~p:~p~n~p:~p(~p, ~p, State)",
                      [_T, _Err,
                       erlang:get_stacktrace(), %% Absent from non-test bytecode.
                       ?MODULE, ?FUNCTION_NAME, Value, Data]),
            %% TODO: Better error for illegal call.
            {error, illegal_primop_call}
    end.

%% ------------------------------------------------------------------
%% Basic account operations.
%% ------------------------------------------------------------------

spend_call(Value, Data, State) ->
    [Recipient] = get_args([word], Data),
    %% TODO: This assumes that we are spending to an account
    RecipientId = aec_id:create(account, <<Recipient:256>>),
    Callback = fun(API, ChainState) ->
                       API:spend(RecipientId, Value, ChainState) end,
    cast_chain(Callback, State).

%% ------------------------------------------------------------------
%% Oracle operations.
%% ------------------------------------------------------------------

oracle_call(?PRIM_CALL_ORACLE_REGISTER, Value, Data, State) ->
    oracle_call_register(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_QUERY, Value, Data, State) ->
    oracle_call_query(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_RESPOND, Value, Data, State) ->
    oracle_call_respond(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_EXTEND, Value, Data, State) ->
    oracle_call_extend(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_ANSWER, Value, Data, State) ->
    oracle_call_get_answer(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_GET_QUESTION, Value, Data, State) ->
    oracle_call_get_question(Value, Data, State);
oracle_call(?PRIM_CALL_ORACLE_QUERY_FEE, Value, Data, State) ->
    oracle_call_query_fee(Value, Data, State);
oracle_call(PrimOp, _, _, _) ->
    {error, {illegal_oracle_primop_call, PrimOp}}.

call_chain1(Callback, State) ->
    Callback(State#chain.api, State#chain.state).

query_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, Res} ->
            Return = {ok, aeso_data:to_binary(Res)},
            {ok, Return, 0, State#chain.state};
        {error, _} = Err -> Err
    end.

cast_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, ChainState1} ->
            UnitReturn = {ok, <<0:256>>},
            GasSpent   = 0,         %% Already costs lots of gas
            {ok, UnitReturn, GasSpent, ChainState1};
        {error, _} = Err -> Err
    end.

call_chain(Callback, State) ->
    case call_chain1(Callback, State) of
        {ok, Retval, ChainState1} ->
            GasSpent   = 0,         %% Already costs lots of gas
            Return     = {ok, aeso_data:to_binary(Retval)},
            {ok, Return, GasSpent, ChainState1};
        {error, _} = Err -> Err
    end.

%% Sophia representation of aeo_oracles:ttl().
oracle_ttl_t() ->
    {variant_t, [{delta, [word]}, {block, [word]}]}.

oracle_call_register(_Value, Data, State) ->
    ArgumentTypes = [word, word, word, oracle_ttl_t(), typerep, typerep],
    [Acct, Sign, QFee, TTL, QType, RType] = get_args(ArgumentTypes, Data),
    Callback =
        fun(API, ChainState) ->
            case API:oracle_register(<<Acct:256>>, <<Sign:256>>, QFee, TTL, QType, RType, ChainState) of
                {ok, <<OKey:256>>, ChainState1} -> {ok, OKey, ChainState1};
                {error, _} = Err                -> Err
            end end,
    call_chain(Callback, State).

oracle_call_query(Value, Data, State) ->
    [Oracle]  = get_args([word], Data),  %% We need the oracle address before we can decode the query
    OracleKey = <<Oracle:256>>,
    case call_chain1(fun(API, ChainState) -> API:oracle_query_format(OracleKey, ChainState) end, State) of
        {ok, QueryType} ->
            ArgumentTypes = [word, QueryType, oracle_ttl_t(), oracle_ttl_t()],
            [_Oracle, Q, QTTL, RTTL] = get_args(ArgumentTypes, Data),
            Callback = fun(API, ChainState) ->
                case API:oracle_query(OracleKey, Q, _QFee=Value, QTTL, RTTL, ChainState) of
                    {ok, <<QKey:256>>, ChainState1} -> {ok, QKey, ChainState1};
                    {error, _} = Err                -> Err
                end end,
            call_chain(Callback, State);
        {error, _} = Err -> Err
    end.


oracle_call_respond(_Value, Data, State) ->
    [Oracle, Query] = get_args([word, word], Data),
    OracleKey = <<Oracle:256>>,
    case call_chain1(fun(API, ChainState) -> API:oracle_response_format(OracleKey, ChainState) end, State) of
        {ok, RType} ->
            ArgumentTypes = [word, word, word, RType],
            [_, _, Sign, R] = get_args(ArgumentTypes, Data),
            QueryKey = <<Query:256>>,
            Callback = fun(API, ChainState) -> API:oracle_respond(OracleKey, QueryKey, Sign, R, ChainState) end,
            cast_chain(Callback, State);
        {error, _} = Err -> Err
    end.


oracle_call_extend(_Value, Data, State) ->
    ArgumentTypes = [word, word, oracle_ttl_t()],
    [Oracle, Sign, TTL] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_extend(<<Oracle:256>>, Sign, TTL, ChainState) end,
    cast_chain(Callback, State).


oracle_call_get_answer(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_answer(<<O:256>>, <<Q:256>>, ChainState) end,
    query_chain(Callback, State).


oracle_call_get_question(_Value, Data, State) ->
    ArgumentTypes = [word, word],
    [O, Q] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_get_question(<<O:256>>, <<Q:256>>, ChainState) end,
    query_chain(Callback, State).


oracle_call_query_fee(_Value, Data, State) ->
    ArgumentTypes = [word],
    [Oracle] = get_args(ArgumentTypes, Data),
    Callback = fun(API, ChainState) -> API:oracle_query_fee(<<Oracle:256>>, ChainState) end,
    query_chain(Callback, State).

%% ------------------------------------------------------------------
%% AENS operations.
%% ------------------------------------------------------------------

aens_call(?PRIM_CALL_AENS_RESOLVE, _Value, Data, State) ->
    aens_call_resolve(Data, State);
aens_call(?PRIM_CALL_AENS_PRECLAIM, _Value, Data, State) ->
    aens_call_preclaim(Data, State);
aens_call(?PRIM_CALL_AENS_CLAIM, _Value, Data, State) ->
    aens_call_claim(Data, State);
aens_call(?PRIM_CALL_AENS_TRANSFER, _Value, Data, State) ->
    aens_call_transfer(Data, State);
aens_call(?PRIM_CALL_AENS_REVOKE, _Value, Data, State) ->
    aens_call_revoke(Data, State);
aens_call(PrimOp, _, _, _) ->
    {error, {illegal_aens_primop_call, PrimOp}}.

aens_call_resolve(Data, State) ->
    [Name, Key, Type] = get_args([string, string, typerep], Data),
    Callback = fun(API, ChainState) -> API:aens_resolve(Name, Key, Type, ChainState) end,
    query_chain(Callback, State).

aens_call_preclaim(Data, State) ->
    [Addr, CHash, Sign] = get_args([word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_preclaim(<<Addr:256>>, <<CHash:256>>, <<Sign:256>>, ChainState) end,
    cast_chain(Callback, State).

aens_call_claim(Data, State) ->
    [Addr, Name, Salt, Sign] = get_args([word, string, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_claim(<<Addr:256>>, Name, Salt, <<Sign:256>>, ChainState) end,
    cast_chain(Callback, State).

aens_call_transfer(Data, State) ->
    [From, To, Hash, Sign] = get_args([word, word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_transfer(<<From:256>>, <<To:256>>, <<Hash:256>>, <<Sign:256>>, ChainState) end,
    cast_chain(Callback, State).

aens_call_revoke(Data, State) ->
    [Addr, Hash, Sign] = get_args([word, word, word], Data),
    Callback = fun(API, ChainState) -> API:aens_revoke(<<Addr:256>>, <<Hash:256>>, <<Sign:256>>, ChainState) end,
    cast_chain(Callback, State).

%% ------------------------------------------------------------------
%% Map operations.
%% ------------------------------------------------------------------

map_call(?PRIM_CALL_MAP_EMPTY, _Value, Data, State) ->
    map_call_empty(Data, State);
map_call(?PRIM_CALL_MAP_GET, _Value, Data, State) ->
    map_call_get(Data, State);
map_call(?PRIM_CALL_MAP_PUT, _Value, Data, State) ->
    map_call_put(Data, State);
map_call(?PRIM_CALL_MAP_DELETE, _Value, Data, State) ->
    map_call_delete(Data, State);
map_call(?PRIM_CALL_MAP_SIZE, _Value, Data, State) ->
    map_call_size(Data, State);
map_call(?PRIM_CALL_MAP_TOLIST, _Value, Data, State) ->
    map_call_tolist(Data, State);
map_call(_, _, _, _) ->
    {error, out_of_gas}.

map_call_empty(Data, State) ->
    [KeyType, ValType] = get_args([typerep, typerep], Data),
    {MapId, State1} = aevm_eeevm_maps:empty(KeyType, ValType, State),
    {ok, {ok, <<MapId:32/unit:8>>}, 0, State1}.

map_call_size(Data, State) ->
    [MapId] = get_args([word], Data),
    Size = aevm_eeevm_maps:size(MapId, State),
    {ok, {ok, <<Size:256>>}, 0, State}.

map_call_get(Data, State) ->
    [MapId]   = get_args([word], Data),
    {KeyType, _ValType} = aevm_eeevm_maps:map_type(MapId, State),
    [_, KeyPtr]  = get_args([word, word], Data),
    {ok, KeyBin} = aevm_eeevm_state:heap_to_binary(KeyType, KeyPtr, State),
    Res = case aevm_eeevm_maps:get(MapId, KeyBin, State) of
            false -> aeso_data:to_binary(none);
            <<ValPtr:256, ValBin/binary>> ->
                %% Some hacky juggling to build an option value.
                NewPtr = 32 + byte_size(ValBin),
                <<NewPtr:256, ValBin/binary, 1:256, ValPtr:256>>
          end,
    {ok, {ok, Res}, 0, State}.

map_call_put(Data, State) ->
    [MapId]             = get_args([word], Data),
    {KeyType, ValType}  = aevm_eeevm_maps:map_type(MapId, State),
    [_, KeyPtr, ValPtr] = get_args([word, word, word], Data),
    {ok, KeyBin}        = aevm_eeevm_state:heap_to_binary(KeyType, KeyPtr, State),
    {ok, ValBin}        = aevm_eeevm_state:heap_to_heap(ValType, ValPtr, State),
    {NewMapId, State1}  = aevm_eeevm_maps:put(MapId, KeyBin, ValBin, State),
    {ok, {ok, <<NewMapId:256>>}, 0, State1}.

map_call_delete(Data, State) ->
    [MapId]      = get_args([word], Data),
    {KeyType, _} = aevm_eeevm_maps:map_type(MapId, State),
    [_, KeyPtr]  = get_args([word, word], Data),
    {ok, KeyBin} = aevm_eeevm_state:heap_to_binary(KeyType, KeyPtr, State),
    {NewMapId, State1} = aevm_eeevm_maps:delete(MapId, KeyBin, State),
    {ok, {ok, <<NewMapId:256>>}, 0, State1}.

map_call_tolist(Data, State) ->
    [MapId] = get_args([word], Data),
    {KeyType, ValType} = aevm_eeevm_maps:map_type(MapId, State),
    {ok, Map} = aevm_eeevm_maps:get_flat_map(MapId, State),
    List = maps:to_list(Map),
    HeapBin = build_heap_list(aevm_eeevm_state:maps(State), KeyType, ValType, List),
    {ok, {ok, HeapBin}, 0, State}.

build_heap_list(_, _, _, []) -> <<(-1):256>>;
build_heap_list(Maps, KeyType, ValType, KVs) ->
    build_heap_list(Maps, KeyType, ValType, KVs, 32, []).

build_heap_list(_, _, _, [], _, Acc) ->
    <<32:256, (list_to_binary(lists:reverse(Acc)))/binary>>;
build_heap_list(Maps, KeyType, ValType, [{K, V} | KVs], Offs, Acc) ->
    %% Addr:  Offs      Offs + 32  HeadPtr  HeadPtr + 32  KeyPtr    ValPtr  TailPtr
    %% Data:  [HeadPtr] [TailPtr]  [KeyPtr]   [ValPtr]    KeyBin    ValBin  NextConsCell
    HeadPtr      = Offs + 64,
    KeyPtr       = HeadPtr + 64,
    NextId       = 0,   %% There are no maps in map keys
    {ok, KeyVal} = aeso_data:binary_to_heap(KeyType, K, NextId, KeyPtr),
    KeyBin       = aeso_data:heap_value_heap(KeyVal),
    KeyPtr1      = aeso_data:heap_value_pointer(KeyVal),
    ValPtr       = KeyPtr + byte_size(KeyBin),
    <<VP:256, VB/binary>> = V,
    {ok, ValVal} = aeso_data:heap_to_heap(ValType, aeso_data:heap_value(Maps, VP, VB, 32), ValPtr),
    ValPtr1      = aeso_data:heap_value_pointer(ValVal),
    ValBin       = aeso_data:heap_value_heap(ValVal),
    TailPtr      =
        case KVs of
            [] -> -1;
            _  -> ValPtr + byte_size(ValBin)
        end,
    ConsCell = <<HeadPtr:256, TailPtr:256>>,
    PairCell = <<KeyPtr1:256, ValPtr1:256>>,
    build_heap_list(Maps, KeyType, ValType, KVs, TailPtr,
                    [[ConsCell, PairCell, KeyBin, ValBin] | Acc]).


%% ------------------------------------------------------------------
%% Internal functions
%% ------------------------------------------------------------------

get_primop(Data) ->
    {ok, {_, T}} = aeso_data:from_binary({tuple, [word, {tuple, [word]}]}, Data),
    {PrimOp} = T,
    PrimOp.

get_args(Types, Data) ->
    {ok, {_, Val}} = aeso_data:from_binary({tuple, [word, {tuple, [word | Types]}]}, Data),
    [_ | Args] = tuple_to_list(Val),
    Args.

