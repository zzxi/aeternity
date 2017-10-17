%%%-------------------------------------------------------------------
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc Tests for Recursive Length Prefix
%%% @end
%%%-------------------------------------------------------------------

-module(aevm_rlp_tests).

-include_lib("eunit/include/eunit.hrl").

rlp_one_byte_test() ->
    <<42>> = aevm_rlp:rlp(<<42>>).

rlp_another_one_byte_test() ->
    B = 127,
    <<B>> = aevm_rlp:rlp(<<B>>).

rlp_two_bytes_test() ->
    B = 128,
    X = <<B>>,
    <<129, B>> = aevm_rlp:rlp(X).
    
rlp_many_bytes_test() ->
    L = 55,
    X = list_to_binary(lists:seq(1,L)),
    <<183, X/binary>> = aevm_rlp:rlp(X).

rlp_some_more_bytes_test() ->
    L = 56,
    X = list_to_binary(lists:duplicate(L, 42)),
    <<184, 56, X/binary>> = aevm_rlp:rlp(X).

rlp_many_more_bytes_test() ->
    L = 256,
    X = list_to_binary(lists:duplicate(L, 42)),
    <<185, 1, 0, X/binary>> = aevm_rlp:rlp(X).

rlp_just_one_more_bytes_test() ->
    L = 257,
    X = list_to_binary(lists:duplicate(L, 42)),
    <<185, 1, 1, X/binary>> = aevm_rlp:rlp(X).

rlp_onebytelist_test() ->
    L = 1,
    X = lists:duplicate(L, 42),
    <<193, 42>> = aevm_rlp:rlp(X).

rlp_shortbytelist_test() ->
    L = 55,
    X = lists:duplicate(L, 42),
    <<247, 42, B/binary>> = aevm_rlp:rlp(X),
    54 = byte_size(B).

%% TODO: see decode comment in aevm_rlp.erl
%% rlp_longbytelist_test() ->
%%     L = 56,
%%     X = lists:duplicate(L, 42),
%%     <<248, 56, 42, B/binary>> = aevm_rlp:rlp(X),
%%     55 = byte_size(B).
    

rlp_integer_test() ->
    X = 1,
    <<1>> = aevm_rlp:rlp(X).

rlp_twobyteinteger_test() ->
    X = 256,
    <<130,1,0>> = aevm_rlp:rlp(X).

rlp_largeinteger_test() ->
    X = 1 bsl 255,
    <<160,128,0, B/binary>> = aevm_rlp:rlp(X),
    30 = byte_size(B).

rlp_verylargeinteger_test() ->
    X = 1 bsl 2555,
    <<185,1, 64, 8, 0, B/binary>> = aevm_rlp:rlp(X),
    318 = byte_size(B).


rlp_listofints_test() ->
    X = [1,256,1 bsl 255, 1 bsl 2555],
    <<249, 1, 104, 1, 130, 1, 0, 160, 128, B/binary>> = aevm_rlp:rlp(X),
    354 = byte_size(B).
    
rlp_decode_one_byte_test() ->
    <<42>> = aevm_rlp:decode(aevm_rlp:rlp(<<42>>)).

rlp_decode_another_one_byte_test() ->
    B = 127,
    <<B>> = aevm_rlp:decode(aevm_rlp:rlp(<<B>>)).

rlp_decode_two_bytes_test() ->
    B = 128,
    X = <<B>>,
    X = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_many_bytes_test() ->
    L = 55,
    X = list_to_binary(lists:seq(1,L)),
    X = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_some_more_bytes_test() ->
    L = 56,
    X = list_to_binary(lists:duplicate(L, 42)),
    X = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_many_more_bytes_test() ->
    L = 256,
    X = list_to_binary(lists:duplicate(L, 42)),
    X = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_just_one_more_bytes_test() ->
    L = 257,
    X = list_to_binary(lists:duplicate(L, 42)),
    X = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_onebytelist_test() ->
    L = 1,
    X = lists:duplicate(L, 42),
    X = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_shortbytelist_test() ->
    L = 55,
    X = lists:duplicate(L, 42),
    X = aevm_rlp:decode(aevm_rlp:rlp(X)).


%% TODO: see decode comment in aevm_rlp.erl
%% rlp_decode_longbytelist_test() ->
%%     L = 56,
%%     X = lists:duplicate(L, 42),
%%     X = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_integer_test() ->
    X = 1,
    <<X/unsigned-integer>> = aevm_rlp:decode(aevm_rlp:rlp(X)).

rlp_decode_twobyteinteger_test() ->
    X = 256,
    R = aevm_rlp:decode(aevm_rlp:rlp(X)),
    S = byte_size(R) * 8, 
    <<X:S/unsigned-integer>> = R.

rlp_decode_largeinteger_test() ->
    X = 1 bsl 255,
    R = aevm_rlp:decode(aevm_rlp:rlp(X)),
    S = byte_size(R) * 8, 
    <<X:S/unsigned-integer>> = R.

rlp_decode_verylargeinteger_test() ->
    X = 1 bsl 2555,
    R = aevm_rlp:decode(aevm_rlp:rlp(X)),
    S = byte_size(R) * 8, 
    <<X:S/unsigned-integer>> = R.


%% TODO: see decode comment in aevm_rlp.erl
%% rlp_decode_listofints_test() ->
%%     X = [1,256,1 bsl 255, 1 bsl 2555],
%%     X = aevm_rlp:decode(aevm_rlp:rlp(X)).

