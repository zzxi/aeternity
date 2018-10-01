-module(aeso_data).

-export([ to_binary/1
        , to_binary/2
        , binary_to_words/1
        , from_heap/3
        , binary_to_heap/3
        , heap_to_binary/2
        , heap_value/3
        , heap_value/4
        , heap_value_pointer/1
        , heap_value_offset/1
        , heap_value_heap/1
        , from_binary/2
        , from_binary/3
        , get_function_from_calldata/1
        , sophia_type_to_typerep/1
        ]).

-export_type([binary_value/0, heap_value/0, heap_fragment/0]).

-include("aeso_icode.hrl").
-include("aeso_data.hrl").

-record(heap, { maps   :: #maps{} | aevm_eeevm_maps:maps(), %% There are really the same
                offset :: offset(),
                heap   :: binary() }).

-type word()            :: non_neg_integer().
-type pointer()         :: word().
-type offset()          :: non_neg_integer().
-type binary_value()    :: binary().
-opaque heap_fragment() :: #heap{}.
-type heap_value()      :: {pointer(), heap_fragment()}.

%% -- Manipulating heap values -----------------------------------------------

no_maps() -> aevm_eeevm_maps:init_maps().

heap_fragment(Maps, Offs, Heap) ->
    #heap{maps = Maps, offset = Offs, heap = Heap}.

heap_fragment(Offs, Heap) ->
    heap_fragment(no_maps(), Offs, Heap).

-spec heap_value(aevm_eeevm_maps:maps(), pointer(), binary(), offset()) -> heap_value().
heap_value(Maps, Ptr, Heap, Offs) ->
    {Ptr, heap_fragment(Maps, Offs, Heap)}.

-spec heap_value(aevm_eeevm_maps:maps(), pointer(), binary()) -> heap_value().
heap_value(Maps, Ptr, Heap) ->
    heap_value(Maps, Ptr, Heap, 0).

-spec heap_value_pointer(heap_value()) -> pointer().
heap_value_pointer({Ptr, _}) -> Ptr.

-spec heap_value_offset(heap_value()) -> offset().
heap_value_offset({_, Heap}) -> Heap#heap.offset.

-spec heap_value_heap(heap_value()) -> binary().
heap_value_heap({_, Heap}) -> Heap#heap.heap.

%% -- Binary to heap ---------------------------------------------------------

-spec binary_to_heap(Type :: ?Type(), Bin :: binary_value(), Offs :: offset()) ->
        {ok, heap_value()} | {error, term()}.
%% Takes a binary encoded with to_binary/1 and returns a heap fragment starting at Offs.
binary_to_heap(Type, <<Ptr:32/unit:8, Heap/binary>>, Offs) ->
    try
        {Addr, _, Mem} = convert(binary, heap, #{}, Type, Ptr, heap_fragment(32, Heap), Offs),
        Maps = no_maps(), %% TODO
        {ok, heap_value(Maps, Addr, list_to_binary(Mem), Offs)}
    catch _:Err ->
        {error, {Err, erlang:get_stacktrace()}}
    end;
binary_to_heap(_Type, <<>>, _Offs) ->
    {error, binary_too_short}.

%% -- Heap to binary ---------------------------------------------------------

-spec heap_to_binary(Type :: ?Type(), Heap :: heap_value()) ->
        {ok, binary_value()} | {error, term()}.
heap_to_binary(Type, {Ptr, Heap}) ->
    try
        {Addr, _, Memory} = convert(heap, binary, #{}, Type, Ptr, Heap, 32),
        {ok, <<Addr:256, (list_to_binary(Memory))/binary>>}
    catch _:Err ->
        {error, {Err, erlang:get_stacktrace()}}
    end.

%% -- Generic heap/binary conversion -----------------------------------------

-type visited() :: #{pointer() => true}.
-type format() :: heap | binary.

-spec convert(Input :: format(), Output :: format(),
                     visited(), ?Type(), pointer(),
                     heap_fragment(), offset()) -> {pointer(), offset(), [iodata()]}.
convert(_, _, _, word, Val, _Heap, _) ->
    {Val, 0, []};
convert(_, _, _Visited, string, Val, Heap, BaseAddr) ->
    Size  = get_word(Heap, Val),
    Words = 1 + (Size + 31) div 32, %% 1 + ceil(Size / 32)
    {BaseAddr, Words * 32, [get_chunk(Heap, Val, Words)]};
convert(Input, Output, Visited, {list, T}, Val, Heap, BaseAddr) ->
    <<Nil:256>> = <<(-1):256>>,   %% empty list is -1
    case Val of
        Nil -> {Nil, 0, []};
        _   -> convert(Input, Output, Visited, {tuple, [T, {list, T}]}, Val, Heap, BaseAddr)
    end;
convert(heap, binary, _Visited, {pmap, _K, _V}, MapId, Heap, BaseAddr) ->
    #{ MapId := Map } = Heap#heap.maps#maps.maps,
    %% TODO: deal with stored maps and tombstones
    KVs  = maps:to_list(Map#pmap.data),
    Size = maps:size(Map#pmap.data),
    {RMem, FinalBase} =
        lists:foldl(fun({Key, Val}, {Mem, Base}) ->
                        KeySize = byte_size(Key),
                        ValSize = byte_size(Val),
                        Base1   = Base + KeySize + 32 + ValSize + 32,
                        {[[<<KeySize:256>>, Key, <<ValSize:256>>, Val] | Mem],
                         Base1}
                    end, {[], BaseAddr + 32}, KVs),
    Mem  = lists:reverse(RMem),
    {BaseAddr, FinalBase - BaseAddr, [<<Size:256>>, Mem]};
convert(_Input, _Output, _Visited, {pmap, _K, _V}, Val, _Heap, _BaseAddr) ->
    {Val, 0, []};   %% TODO: depends on Input/Output
convert(_, _, _, {tuple, []}, _Ptr, _Heap, _BaseAddr) ->
    {0, 0, []}; %% Use 0 for the empty tuple (need a unique value).
convert(Input, Output, Visited, {tuple, Ts}, Ptr, Heap, BaseAddr) ->
    Visited1  = visit(Visited, Ptr),
    BaseAddr1 = BaseAddr + 32 * length(Ts),  %% store component data after the tuple cell
    Ptrs      = [ P || <<P:256>> <= get_chunk(Heap, Ptr, length(Ts)) ],
    {BaseAddr2, NewPtrs, Memory} = convert_components(Input, Output, Visited1, Ts, Ptrs, Heap, BaseAddr1),
    {BaseAddr, BaseAddr2 - BaseAddr, [NewPtrs, Memory]};
convert(Input, Output, Visited, {variant, Cs}, Ptr, Heap, BaseAddr) ->
    Tag = get_word(Heap, Ptr),
    Ts  = lists:nth(Tag + 1, Cs),
    convert(Input, Output, Visited, {tuple, [word | Ts]}, Ptr, Heap, BaseAddr);
convert(Input, Output, Visited, typerep, Ptr, Heap, BaseAddr) ->
    Typerep = {variant, [[],                         %% word
                         [],                         %% string
                         [typerep],                  %% list
                         [{list, typerep}],          %% tuple
                         [{list, {list, typerep}}],  %% variant
                         [],                         %% typerep
                         [typerep, typerep]          %% map
                        ]},
    convert(Input, Output, Visited, Typerep, Ptr, Heap, BaseAddr).

convert_components(Input, Output, Visited, Ts, Ps, Heap, BaseAddr) ->
    convert_components(Input, Output, Visited, Ts, Ps, Heap, BaseAddr, [], []).

convert_components(_, _, _Visited, [], [], _Heap, BaseAddr, PtrAcc, MemAcc) ->
    {BaseAddr, lists:reverse(PtrAcc), lists:reverse(MemAcc)};
convert_components(Input, Output, Visited, [T | Ts], [Ptr | Ptrs], Heap, BaseAddr, PtrAcc, MemAcc) ->
    {NewPtr, Size, Mem} = convert(Input, Output, Visited, T, Ptr, Heap, BaseAddr),
    convert_components(Input, Output, Visited, Ts, Ptrs, Heap, BaseAddr + Size,
                         [<<NewPtr:256>> | PtrAcc], [Mem | MemAcc]).

%% -- Value to binary --------------------------------------------------------

-spec to_binary(aeso_sophia:data()) -> aeso_sophia:heap().
%% Encode the data as a heap where the first word is the value (for unboxed
%% types) or a pointer to the value (for boxed types).
to_binary(Data) ->
    to_binary(Data, 0).

to_binary(Data, BaseAddress) ->
    {Address, Memory} = to_binary1(Data, BaseAddress + 32),
    R = <<Address:256, Memory/binary>>,
    R.


%% Allocate the data in memory, from the given address.  Return a pair
%% of memory contents from that address and the value representing the
%% data.
to_binary1(Data,_Address) when is_integer(Data) ->
    {Data,<<>>};
to_binary1(Data, Address) when is_binary(Data) ->
    %% a string
    Words = binary_to_words(Data),
    {Address,<<(size(Data)):256, << <<W:256>> || W <- Words>>/binary>>};
to_binary1(none, Address)            -> to_binary1({variant, 0, []}, Address);
to_binary1({some, Value}, Address)   -> to_binary1({variant, 1, [Value]}, Address);
to_binary1(word, Address)            -> to_binary1({?TYPEREP_WORD_TAG}, Address);
to_binary1(string, Address)          -> to_binary1({?TYPEREP_STRING_TAG}, Address);
to_binary1(typerep, Address)         -> to_binary1({?TYPEREP_TYPEREP_TAG}, Address);
to_binary1({list, T}, Address)       -> to_binary1({?TYPEREP_LIST_TAG, T}, Address);
to_binary1({option, T}, Address)     -> to_binary1({variant, [[], [T]]}, Address);
to_binary1({tuple, Ts}, Address)     -> to_binary1({?TYPEREP_TUPLE_TAG, Ts}, Address);
to_binary1({variant, Cons}, Address) -> to_binary1({?TYPEREP_VARIANT_TAG, Cons}, Address);
to_binary1({pmap, K, V}, Address)    -> to_binary1({?TYPEREP_MAP_TAG, K, V}, Address);
to_binary1({variant, Tag, Args}, Address) ->
    to_binary1(list_to_tuple([Tag | Args]), Address);
to_binary1({pmap, Map}, Address) -> %% TODO: Remove tag when replacing map with pmap
    Size = maps:size(Map),
    %% Sort according to binary ordering
    KVs = lists:sort([ {to_binary(K), to_binary(V)} || {K, V} <- maps:to_list(Map) ]),
    {Address, <<Size:256, << <<(byte_size(K)):256, K/binary,
                               (byte_size(V)):256, V/binary>> || {K, V} <- KVs >>/binary >>};
to_binary1(Map, Address) when is_map(Map) ->
    to_binary1(maps:to_list(Map), Address);
to_binary1({}, _Address) ->
    {0, <<>>};
to_binary1(Data, Address) when is_tuple(Data) ->
    {Elems,Memory} = to_binaries(tuple_to_list(Data),Address+32*size(Data)),
    ElemsBin = << <<W:256>> || W <- Elems>>,
    {Address,<< ElemsBin/binary, Memory/binary >>};
to_binary1([],_Address) ->
    <<Nil:256>> = <<(-1):256>>,
    {Nil,<<>>};
to_binary1([H|T],Address) ->
    to_binary1({H,T},Address).


to_binaries([],_Address) ->
    {[],<<>>};
to_binaries([H|T],Address) ->
    {HRep,HMem} = to_binary1(H,Address),
    {TRep,TMem} = to_binaries(T,Address+size(HMem)),
    {[HRep|TRep],<<HMem/binary, TMem/binary>>}.

binary_to_words(<<>>) ->
    [];
binary_to_words(<<N:256,Bin/binary>>) ->
    [N|binary_to_words(Bin)];
binary_to_words(Bin) ->
    binary_to_words(<<Bin/binary,0>>).

%% Interpret a return value (a binary) using a type rep.

-spec from_heap(Type :: ?Type(), Heap :: binary(), Ptr :: integer()) ->
        {ok, term()} | {error, term()}.
from_heap(Type, Heap, Ptr) ->
    try {ok, from_binary(#{}, Type, Heap, Ptr)}
    catch _:Err ->
        {error, {Err, erlang:get_stacktrace()}}
    end.

%% Base address is the address of the first word of the given heap.
-spec from_binary(T :: ?Type(),
                  Heap :: binary(),
                  BaseAddr :: non_neg_integer()) ->
        {ok, term()} | {error, term()}.
from_binary(T, Heap = <<V:256, _/binary>>, BaseAddr) ->
    from_heap(T, <<0:BaseAddr/unit:8, Heap/binary>>, V);
from_binary(_, Bin, _BaseAddr) ->
    {error, {binary_too_short, Bin}}.

-spec from_binary(?Type(), binary()) -> {ok, term()} | {error, term()}.
from_binary(T, Heap) ->
    from_binary(T, Heap, 0).

from_binary(_, word, _, V) ->
    V;
from_binary(_, signed_word, _, V) ->
    <<N:256/signed>> = <<V:256>>,
    N;
from_binary(_, bool, _, V) ->
    case V of
        0 -> false;
        1 -> true
    end;
from_binary(_, string, Heap, V) ->
    StringSize = heap_word(Heap,V),
    BitAddr = 8*(V+32),
    <<_:BitAddr,Bytes:StringSize/binary,_/binary>> = Heap,
    Bytes;
from_binary(_, {tuple, []}, _, _) ->
    {};
from_binary(Visited, {tuple,Cpts}, Heap, V) ->
    check_circular_refs(Visited, V),
    NewVisited = Visited#{V => true},
    ElementNums = lists:seq(0, length(Cpts)-1),
    TypesAndPointers = lists:zip(Cpts, ElementNums),
    ElementAddress = fun(Index) -> V + 32 * Index end,
    Element = fun(Index) ->
                      heap_word(Heap, ElementAddress(Index))
              end,
    Convert = fun(Type, Index) ->
                from_binary(NewVisited, Type, Heap, Element(Index))
              end,
    Elements = [Convert(T, I) || {T,I} <- TypesAndPointers],
    list_to_tuple(Elements);
from_binary(Visited, {list, Elem}, Heap, V) ->
    <<Nil:256>> = <<(-1):256>>,
    if V==Nil ->
          [];
       true ->
          {H,T} = from_binary(Visited, {tuple,[Elem,{list,Elem}]},Heap,V),
          [H|T]
    end;
from_binary(Visited, {option, A}, Heap, V) ->
    from_binary(Visited, {variant_t, [{none, []}, {some, [A]}]}, Heap, V);
from_binary(Visited, {variant, Cons}, Heap, V) ->
    Tag      = heap_word(Heap, V),
    Args     = lists:nth(Tag + 1, Cons),
    Visited1 = Visited#{V => true},
    {variant, Tag, tuple_to_list(from_binary(Visited1, {tuple, Args}, Heap, V + 32))};
from_binary(Visited, {variant_t, TCons}, Heap, V) ->   %% Tagged variants
    {Tags, Cons} = lists:unzip(TCons),
    {variant, I, Args} = from_binary(Visited, {variant, Cons}, Heap, V),
    Tag = lists:nth(I + 1, Tags),
    case Args of
        []  -> Tag;
        _   -> list_to_tuple([Tag | Args])
    end;
from_binary(_Visited, {pmap, A, B}, Heap, Ptr) ->
    %% FORMAT: [Size] [KeySize] Key [ValSize] Val .. [KeySize] Key [ValSize] Val
    Size = heap_word(Heap, Ptr),
    map_from_binary(A, B, Size, Heap, Ptr + 32);
from_binary(Visited, {map, A, B}, Heap, V) ->
    maps:from_list(from_binary(Visited, {list, {tuple, [A, B]}}, Heap, V));
from_binary(Visited, typerep, Heap, V) ->
    check_circular_refs(Visited, V),
    Tag = heap_word(Heap, V),
    Arg1 = fun(T, I) -> from_binary(Visited#{V => true}, T, Heap, heap_word(Heap, V + 32 * I)) end,
    Arg  = fun(T) -> Arg1(T, 1) end,
    case Tag of
        ?TYPEREP_WORD_TAG    -> word;
        ?TYPEREP_STRING_TAG  -> string;
        ?TYPEREP_TYPEREP_TAG -> typerep;
        ?TYPEREP_LIST_TAG    -> {list,   Arg(typerep)};
        ?TYPEREP_TUPLE_TAG   -> {tuple,  Arg({list, typerep})};
        ?TYPEREP_VARIANT_TAG -> {variant, Arg({list, {list, typerep}})};
        ?TYPEREP_MAP_TAG     -> {pmap,    Arg(typerep), Arg1(typerep, 2)}
    end.

map_from_binary(KeyType, ValType, N, Heap, Ptr) ->
    %% Avoid looping on bogus sizes
    MaxN = byte_size(Heap) div 64,
    map_from_binary(KeyType, ValType, min(N, MaxN), Heap, Ptr, #{}).

map_from_binary(_, _, 0, _, _, Map) -> Map;
map_from_binary(KeyType, ValType, I, Heap, Ptr, Map) ->
    KeySize = heap_word(Heap, Ptr),
    KeyPtr  = Ptr + 32,
    KeyBin  = heap_chunk(Heap, KeyPtr, KeySize),
    ValSize = heap_word(Heap, KeyPtr + KeySize),
    ValPtr  = KeyPtr + KeySize + 32,
    ValBin  = heap_chunk(Heap, ValPtr, ValSize),
    %% Keys and values are self contained binaries
    {ok, Key} = from_binary(KeyType, KeyBin),
    {ok, Val} = from_binary(ValType, ValBin),
    map_from_binary(KeyType, ValType, I - 1, Heap, ValPtr + ValSize, Map#{Key => Val}).

visit(Visited, V) ->
    check_circular_refs(Visited, V),
    Visited#{ V => true }.

check_circular_refs(Visited, V) ->
    case maps:is_key(V, Visited) of
        true ->  exit(circular_references);
        false -> ok
    end.

heap_word(Heap,Addr) ->
    BitSize = 8*Addr,
    <<_:BitSize,W:256,_/binary>> = Heap,
    W.

heap_chunk(Heap, Addr, Bytes) ->
    BitOffs = 8 * Addr,
    <<_:BitOffs, Chunk:Bytes/binary, _/binary>> = Heap,
    Chunk.

-spec get_word(heap_fragment(), pointer()) -> word().
get_word(#heap{offset = Offs, heap = Mem}, Addr) when Addr >= Offs ->
    BitOffs = (Addr - Offs) * 8,
    <<_:BitOffs, Word:256, _/binary>> = Mem,
    Word.

-spec get_chunk(heap_fragment(), pointer(), non_neg_integer()) -> binary().
get_chunk(#heap{offset = Offs, heap = Mem}, Addr, Words) when Addr >= Offs ->
    BitOffs = (Addr - Offs) * 8,
    Bytes   = Words * 32,
    <<_:BitOffs, Chunk:Bytes/binary, _/binary>> = Mem,
    Chunk.

-spec get_function_from_calldata(Calldata::binary()) ->
                                        {ok, term()} | {error, term()}.
get_function_from_calldata(Calldata) ->
    case from_binary({tuple, [word, {tuple, [string]}]}, Calldata) of
        {ok, {_, {FunctionName}}} ->
            {ok, FunctionName};
        {error, _} = Error -> Error
    end.


sophia_type_to_typerep(String) ->
    {ok, Ast} = aeso_parser:type(String),
    try aeso_ast_to_icode:ast_typerep(Ast) of
        Type -> {ok, Type}
    catch _:_ -> {error, bad_type}
    end.

