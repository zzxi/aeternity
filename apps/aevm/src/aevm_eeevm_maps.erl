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
        , empty/3
        , get/3
        , put/4
        , delete/3
        ]).

-export_type([map_id/0, vm_maps/0]).

-record(vm_maps, { maps    = #{} :: #{ map_id() => pmap() }
                 , next_id = 0   :: integer() }).

-opaque vm_maps() :: #vm_maps{}.

-type state()    :: state().
-type map_id()   :: integer().
-type value()    :: aeso_data:binary_value().
-type map_data() :: #{value() => value() | tombstone}.
-type typerep()  :: aeso_sophia:type().

-record(pmap, {key_t  :: typerep(),
               val_t  :: typerep(),
               parent :: none | map_id(),
               data   :: map_data() | stored}).

-type pmap() :: #pmap{}.

-spec init_maps() -> vm_maps().
init_maps() -> #vm_maps{}.

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
                 data   = #{}},
    add_map(Map, State).

-spec get(map_id(), value(), state()) -> false | value().
get(Id, Key, State) ->
    {ok, Map} = get_map(Id, State),
    case Map#pmap.data of
        #{ Key := Val } ->
            case Val of
                tombstone               -> false;
                Val when is_binary(Val) -> Val
            end;
        _ ->
            case Map#pmap.parent of
                none     -> false;
                ParentId when ParentId < Id -> get(ParentId, Key, State)
                    %% Ensuring termination. ParentId will be smaller than Id
                    %% for any well-formed maps.
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
    case Map#pmap.data of
        Data when is_map(Data) -> %% squash local updates
            add_map(Map#pmap{ data = Data#{Key => Val} }, State);
        stored -> %% not yet implemented
            add_map(Map#pmap{ parent = Id, data = #{Key => Val} }, State)
    end.

%% -- Internal functions -----------------------------------------------------

-spec get_map(map_id(), state()) -> {ok, pmap()} | {error, not_found}.
get_map(MapId, State) ->
    case aevm_eeevm_state:maps(State) of
        #vm_maps{ maps = #{ MapId := Map } } -> {ok, Map};
        _ -> {error, not_found}
    end.

-spec add_map(pmap(), state()) -> {map_id(), state()}.
add_map(Map, State) ->
    Maps    = aevm_eeevm_state:maps(State),
    NewId   = Maps#vm_maps.next_id,
    NewMaps = Maps#vm_maps{ next_id = NewId + 1,
                            maps = (Maps#vm_maps.maps)#{ NewId => Map } },
    {NewId, aevm_eeevm_state:set_maps(NewMaps, State)}.

