%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle store
%%% @end
%%% Created : 9 Oct 2017
%%%-------------------------------------------------------------------

-module(aevm_eeevm_store).

-export([ load/2
        , store/3
        , init/2
        , to_binary/1
        , get_sophia_state/1
        , get_sophia_state_type/1
        , from_sophia_state/1
        , set_sophia_state/2
        , is_valid_key/2
        ]).

-include("aevm_eeevm.hrl").
-include_lib("aecontract/src/aecontract.hrl").
-include_lib("aesophia/src/aeso_data.hrl").

-define(SOPHIA_STATE_KEY,      <<0>>).
-define(SOPHIA_STATE_TYPE_KEY, <<1>>).
-define(SOPHIA_STATE_MAPS_KEY, <<2>>).

%%====================================================================
%% API
%%====================================================================

-spec init(aect_contracts:store(), aevm_eeevm_state:state()) -> aevm_eeevm_state:state().
init(Store, State) -> State#{ storage => binary_to_integer_map(Store) }.

-spec to_binary(aevm_eeevm_state:state()) -> aect_contracts:store().
to_binary(#{ storage := Storage }) -> integer_to_binary_map(Storage).

-spec load(integer(), aevm_eeevm_state:state()) -> integer().
load(Address, State) ->
    Store = aevm_eeevm_state:storage(State),
    Value = storage_read(Address, Store),
    Value.

-spec store(integer(), integer(), aevm_eeevm_state:state()) -> aevm_eeevm_state:state().
store(Address, Value, State) when is_integer(Value) ->
    Store = aevm_eeevm_state:storage(State),
    %% Make sure value fits in 256 bits.
    Value256 = Value band ?MASK256,
    Store1 = storage_write(Address, Value256, Store),
    aevm_eeevm_state:set_storage(Store1, State).

%% The argument should be a binary encoding a pair of a typerep and a value of that type.
-spec from_sophia_state(aeso_data:binary_value()) -> aect_contracts:store().
from_sophia_state(Data) ->
    %% TODO: less encoding/decoding
    {ok, {Type}}    = aeso_data:from_binary({tuple, [typerep]},    Data),
    %% Strip the type from the binary (TODO: temporary)
    Data1 = second_component(Data),
    {ok, StateValue} = aeso_data:binary_to_heap(Type, Data1, 0, 32),
    Maps = maps:to_list((aeso_data:heap_value_maps(StateValue))#maps.maps),
    Mem  = aeso_data:heap_value_heap(StateValue),
    Ptr  = aeso_data:heap_value_pointer(StateValue),
    TypeData = aeso_data:to_binary(Type),
    StateData = <<Ptr:256, Mem/binary>>,
    MapKeys = << <<MapId:256>> || {MapId, _} <- Maps >>,
    MapInfo = maps:from_list(
        [ {<<MapId:256>>, aeso_data:to_binary({Map#pmap.key_t, Map#pmap.val_t, parent_to_word(Map#pmap.parent)})}
          || {MapId, Map} <- Maps ]),
    MapData = maps:from_list(
        [ {<<MapId:256, Key/binary>>, Val}
          || {MapId, #pmap{data = Map}} <- Maps,
             {Key, Val} <- maps:to_list(Map) ]),
    maps:merge(
        #{ ?SOPHIA_STATE_KEY      => StateData,
           ?SOPHIA_STATE_TYPE_KEY => TypeData,
           ?SOPHIA_STATE_MAPS_KEY => MapKeys },
        maps:merge(MapInfo, MapData)).

%% TODO: Temporary hack to drop the first component (the typerep) from the initial state.
-spec second_component(aeso_data:binary_value()) -> aeso_data:binary_value().
second_component(<<Ptr:256, Heap/binary>> = Data) ->
    <<_:Ptr/unit:8, _:256, Snd:256, _/binary>> = Data,
    <<Snd:256, Heap/binary>>.

-spec set_sophia_state(binary(), aect_contracts:store()) -> aect_contracts:store().
set_sophia_state(Data, Store) ->
    %% TODO
    Store#{?SOPHIA_STATE_KEY => Data}.

-spec get_sophia_state(aect_contracts:store()) -> aeso_data:heap_value().
get_sophia_state(Store) ->
    <<Ptr:256, Heap/binary>> = maps:get(?SOPHIA_STATE_KEY, Store, <<>>),
    MapKeys = [ MapId || <<MapId:256>> <= maps:get(?SOPHIA_STATE_MAPS_KEY, Store, <<>>) ],
    Maps = maps:from_list(
        [ begin
              Bin = maps:get(<<MapId:256>>, Store),
              {ok, {KeyT, ValT, ParentWord}} = aeso_data:from_binary({tuple, [typerep, typerep, word]}, Bin),
              Parent = word_to_parent(ParentWord),
              {MapId, #pmap{ key_t = KeyT, val_t = ValT, parent = Parent, data = stored }}
          end || MapId <- MapKeys ]),
    aeso_data:heap_value(#maps{next_id = lists:max([-1 | MapKeys]) + 1, maps = Maps}, Ptr, Heap, 32).

parent_to_word(none) ->
    <<None:256>> = <<(-1):256>>,
    None;
parent_to_word(Id) -> Id.

word_to_parent(Word) ->
    <<None:256>> = <<(-1):256>>,
    if  Word == None -> none;
        true         -> Word
    end.

-spec get_sophia_state_type(aect_contracts:store()) -> false | aeso_sophia:type().
get_sophia_state_type(Store) ->
    case maps:get(?SOPHIA_STATE_TYPE_KEY, Store, false) of
        false -> false;
        Bin   ->
            {ok, Type} = aeso_data:from_binary(typerep, Bin),
            Type
    end.

is_valid_key(?AEVM_01_Sophia_01, ?SOPHIA_STATE_KEY)      -> true;
is_valid_key(?AEVM_01_Sophia_01, ?SOPHIA_STATE_TYPE_KEY) -> true;
is_valid_key(?AEVM_01_Sophia_01, ?SOPHIA_STATE_MAPS_KEY) -> true;
is_valid_key(?AEVM_01_Sophia_01, K) -> is_binary(K) andalso byte_size(K) >= 32;
is_valid_key(?AEVM_01_Solidity_01, K) -> is_binary_map_key(K).

%%====================================================================
%% Internal functions
%%====================================================================

storage_read(Address, Mem) -> maps:get(Address, Mem, 0).

%% No alignment or size check. Don't use directly.
storage_write(Address,     0, Mem) -> maps:remove(Address, Mem);
storage_write(Address, Value, Mem) -> maps:put(Address, Value, Mem).

binary_to_integer_map(ChainStore) ->
    ToInt = fun(K, Val, Map) ->
                    Address = binary_to_integer_map_key(K),
                    case binary:decode_unsigned(Val) of
                        0 -> Map;
                        V -> Map#{ Address => V }
                    end
            end,
    maps:fold(ToInt, #{}, ChainStore).

integer_to_binary_map(Store) ->
    ToBin = fun(A, Val, Map) ->
                    Key = integer_to_binary_map_key(A),
                    case binary:encode_unsigned(Val) of
                        <<0>> -> Map;
                        V -> Map#{ Key => V}
                    end
            end,
    maps:fold(ToBin, #{}, Store).

binary_to_integer_map_key(K) -> binary:decode_unsigned(K).
integer_to_binary_map_key(K) -> binary:encode_unsigned(K).

is_binary_map_key(K) ->
    K =:= integer_to_binary_map_key(binary_to_integer_map_key(K)).
