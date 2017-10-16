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
