%%%-------------------------------------------------------------------
%%% @author Ulf Norell
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Handle AEVM maps.
%%% @end
%%% Created : 28 Sep 2018
%%%-------------------------------------------------------------------
-module(aevm_eeevm_maps).

-export([ map_type/2
        , init_maps/0
        , next_id/1
        , merge/2
        , empty/3
        , size/2
        , get/3
        , put/4
        , delete/3
        , flatten_map/3
        ]).

-include_lib("aesophia/src/aeso_data.hrl").

-export_type([map_id/0, maps/0]).

-type maps() :: #maps{}.

-type state()   :: state().
-type map_id()  :: non_neg_integer().
-type value()   :: aeso_data:binary_value().
-type typerep() :: aeso_sophia:type().

-type pmap() :: #pmap{}.

-spec init_maps() -> maps().
init_maps() -> #maps{}.

-spec next_id(maps()) -> map_id().
next_id(#maps{next_id = Id}) -> Id.

-spec merge(maps(), state()) -> state().
merge(NewMaps, State) ->
    OldMaps = aevm_eeevm_state:maps(State),
    %% Use next_id from NewMaps (unless undefined)
    NextId = NewMaps#maps.next_id,
    Maps = #maps{ maps = maps:merge(OldMaps#maps.maps, NewMaps#maps.maps),
                  next_id = NextId },
    aevm_eeevm_state:set_maps(Maps, State).

-spec map_type(map_id(), state()) -> {typerep(), typerep()}.
map_type(Id, State) ->
    {ok, Map} = get_map(Id, State),
    {Map#pmap.key_t, Map#pmap.val_t}.

-spec empty(typerep(), typerep(), state()) ->
        {map_id(), state()}.
empty(KeyType, ValType, State) ->
    Map = #pmap{ key_t  = KeyType,
                 val_t  = ValType,
                 parent = none,
                 size   = 0,
                 data   = #{}},
    add_map(Map, State).

-spec size(map_id(), state()) -> non_neg_integer().
size(Id, State) ->
    {ok, Map} = get_map(Id, State),
    Map#pmap.size.

-spec get(map_id(), value(), state()) -> false | value().
get(Id, Key, State) ->
    {ok, Map} = get_map(Id, State),
    case Map#pmap.data of
        #{ Key := Val } ->
            case Val of
                tombstone               -> false;
                Val when is_binary(Val) -> Val
            end;
        stored ->
            aevm_eeevm_store:map_lookup(Id, Key, State);
        _ ->
            case Map#pmap.parent of
                none     -> false;  %% Parents are always in the store
                ParentId -> aevm_eeevm_store:map_lookup(ParentId, Key, State)
            end
    end.

-spec put(map_id(), value(), value(), state()) -> {map_id(), state()}.
put(Id, Key, Val, State) ->
    update(Id, Key, Val, State).

-spec delete(map_id(), value(), state()) -> {map_id(), state()}.
delete(Id, Key, State) ->
    update(Id, Key, tombstone, State).

-spec update(map_id(), value(), value() | tombstone, state()) -> {map_id(), state()}.
update(Id, Key, Val, State) ->
    {ok, Map} = get_map(Id, State),
    DeltaSize = delta_size(Id, Key, Val, State),
    case Map#pmap.data of
        Data when is_map(Data) -> %% squash local updates
            Data1 =
                case {Map#pmap.parent, Val} of
                    {none, tombstone} ->  %% skip tombstone if no parent
                        maps:remove(Key, Data);
                    _ -> Data#{Key => Val}
                end,
            add_map(Map#pmap{ size = Map#pmap.size + DeltaSize, data = Data1 }, State);
        stored -> %% not yet implemented
            add_map(Map#pmap{ size = Map#pmap.size + DeltaSize, parent = Id, data = #{Key => Val} }, State)
    end.

delta_size(Id, Key, Val, State) ->
    New = if Val == tombstone -> delete;
             true             -> insert
          end,
    Old = case get(Id, Key, State) of
             false -> absent;
             _     -> present
          end,
    case {New, Old} of
        {delete, absent}  -> 0;
        {delete, present} -> -1;
        {insert, absent}  -> 1;
        {insert, present} -> 0
    end.

%% Follow parent pointers and collapse tombstones.
%% Postcondition: parent == none and no tombstones in values.
%% Use when converting to binary.
-spec flatten_map(aect_contracts:store(), map_id(), pmap()) -> pmap().
flatten_map(Store, MapId, Map) ->
    ParentMap =
        case Map#pmap.parent of
            none   -> #{};
            Parent ->   %% All parents are in the store
                aevm_eeevm_store:get_map_data(Parent, Store)
        end,
    Delta =
        case Map#pmap.data of
            stored -> aevm_eeevm_store:get_map_data(MapId, Store);
            D      -> D
        end,
    Data =
        case Map#pmap.parent of
            none -> Delta;   %% No parent means no tombstones
            _ ->
                lists:foldl(fun({Key, tombstone}, M) -> maps:remove(Key, M);
                               ({Key, Val},       M) -> M#{ Key => Val }
                            end, ParentMap, maps:to_list(Delta))
        end,
    Map#pmap{ parent = none, data = Data }.


%% -- Internal functions -----------------------------------------------------

-spec get_map(map_id(), state()) -> {ok, pmap()} | {error, not_found}.
get_map(MapId, State) ->
    case aevm_eeevm_state:maps(State) of
        #maps{ maps = #{ MapId := Map } } -> {ok, Map};
        _ -> {error, not_found}
    end.

-spec add_map(pmap(), state()) -> {map_id(), state()}.
add_map(Map, State) ->
    Maps    = aevm_eeevm_state:maps(State),
    NewId   = Maps#maps.next_id,
    NewMaps = Maps#maps{ next_id = NewId + 1,
                         maps = (Maps#maps.maps)#{ NewId => Map } },
    {NewId, aevm_eeevm_state:set_maps(NewMaps, State)}.

