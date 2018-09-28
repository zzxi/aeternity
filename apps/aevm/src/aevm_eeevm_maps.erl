%%%-------------------------------------------------------------------
%%% @author Ulf Norell
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle AEVM maps.
%%% @end
%%% Created : 28 Sep 2018
%%%-------------------------------------------------------------------
-module(aevm_eeevm_maps).

-export([ get_map/2
        , map_keytype/1
        , map_valtype/1
        , map_contents/1
        , new_map/4
        ]).

-export_type([map_id/0, pmap/0]).

-type map_id()  :: integer().
-type map_data() :: #{binary() => binary()}.
-type typerep() :: aeso_sophia:type().

-record(pmap, {key_t :: typerep(),
               val_t :: typerep(),
               data  :: map_data()}).

-opaque pmap() :: #pmap{}.

-spec map_keytype(pmap()) -> typerep().
map_keytype(#pmap{key_t = KeyT}) -> KeyT.

-spec map_valtype(pmap()) -> typerep().
map_valtype(#pmap{val_t = ValT}) -> ValT.

-spec map_contents(pmap()) -> map_data().
map_contents(#pmap{data = Data}) -> Data.

-spec get_map(map_id(), aevm_eeevm_state:state()) -> {ok, pmap()} | {error, not_found}.
get_map(MapId, State) ->
    case aevm_eeevm_state:maps(State) of
        #{ MapId := Map } -> {ok, Map};
        _                 -> {error, not_found}
    end.

-spec new_map(typerep(), typerep(), map(), aevm_eeevm_state:state()) ->
        {map_id(), aevm_eeevm_state:state()}.
new_map(KeyType, ValType, Data, State) when is_map(Data) ->
    Maps  = aevm_eeevm_state:maps(State),
    MapId = maps:size(Maps) + 1,
    Map   = #pmap{key_t = KeyType, val_t = ValType, data = Data},
    {MapId, aevm_eeevm_state:set_maps(Maps#{ MapId => Map }, State)}.

