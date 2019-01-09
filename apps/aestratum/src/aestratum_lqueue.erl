-module(aestratum_lqueue).

-export([new/1,
         is_empty/1,
         is_full/1,
         max_len/1,
         len/1,
         to_list/1,
         from_list/2,
         member/2,
         in/2,
         out/1,
         get/1
        ]).

-define(VALID_TYPES(L, M, R, F),
        (is_integer(L) and is_integer(M) and is_list(R) and is_list(F))).

-define(VALID_LENGTH(L, M, R, F),
        ((length(R) + length(F)) =:= L) and (M > 0)).

-define(IS_LQUEUE(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L =< M)).

-define(IS_LQUEUE_FULL(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L =:= M)).

-define(IS_LQUEUE_EMPTY(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L =:= 0)).

-define(IS_LQUEUE_NOT_FULL(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L < M)).

-define(IS_LQUEUE_NOT_EMPTY(L, M, R, F),
        ?VALID_TYPES(L, M, R, F) and
        ?VALID_LENGTH(L, M, R, F) and
        (L > 0) and (L =< M)).

%% API.

new(M) when is_integer(M) and (M > 0) ->
    {0, M, [], []};
new(M) ->
    erlang:error(badarg, [M]).

is_empty({L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    true;
is_empty({L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    false;
is_empty(LQ) ->
    erlang:error(badarg, [LQ]).

is_full({M, M, R, F}) when ?IS_LQUEUE_FULL(M, M, R, F) ->
    true;
is_full({L, M, R, F}) when ?IS_LQUEUE_NOT_FULL(L, M, R, F) ->
    false;
is_full(LQ) ->
    erlang:error(badarg, [LQ]).

max_len({L, M, R, F}) when ?IS_LQUEUE(L, M, R, F) ->
    M;
max_len(LQ) ->
    erlang:error(badarg, [LQ]).

len({L, M, R, F}) when ?IS_LQUEUE(L, M, R, F) ->
    L;
len(LQ) ->
    erlang:error(badarg, [LQ]).

to_list({L, M, R, F}) when ?IS_LQUEUE(L, M, R, F) ->
    F ++ lists:reverse(R, []);
to_list(LQ) ->
    erlang:error(badarg, [LQ]).

from_list([], M) when is_integer(M) and (M > 0) ->
    {0, M, [], []};
from_list(L, M) when
      is_integer(M) and (M > 0) and is_list(L) and (M >= length(L)) ->
    {length(L), M, lists:reverse(L), []};
from_list(L, M) ->
    erlang:error(badarg, [L, M]).

member(_X, {L, M, R, F}) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    false;
member(X, {L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    lists:member(X, R) orelse lists:member(X, F);
member(X, LQ) ->
    erlang:error(badarg, [X, LQ]).

in(X, {L, M, R, F}) when ?IS_LQUEUE_NOT_FULL(L, M, R, F) ->
    {L + 1, M, [X | R], F};
in(X, {L, M, R, [_H | T] = F}) when ?IS_LQUEUE_FULL(L, M, R, F) ->
    {M, M, [X | R], T};
in(X, {L, M, R, [] = F}) when ?IS_LQUEUE_FULL(L, M, R, F) ->
    in(X, {M, M, [], lists:reverse(R)});
in(X, LQ) ->
    erlang:error(badarg, [X, LQ]).

out({L, M, R, F} = LQ) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    {empty, LQ};
out({L, M, R, [H | T] = F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    {{value, H}, {L - 1, M, R, T}};
out({L, M, R, [] = F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    out({L, M, [], lists:reverse(R)});
out(LQ) ->
    erlang:error(badarg, [LQ]).

get({L, M, R, F} = LQ) when ?IS_LQUEUE_EMPTY(L, M, R, F) ->
    {error, empty};
get({L, M, R, F}) when ?IS_LQUEUE_NOT_EMPTY(L, M, R, F) ->
    {ok, get(R, F)};
get(LQ) ->
    erlang:error(badarg, [LQ]).

%% Internal functions

get(_, [FH | _]) ->
    FH;
get([RH], []) ->
    RH;
get([_ | RT], []) ->
    lists:last(RT).

