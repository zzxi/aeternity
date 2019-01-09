-module(aestratum_utils).

-include("aestratum_jsonrpc.hrl").

-export([next_id/1,
         to_id/1,
         max_extra_nonce/1
        ]).

next_id(Id) when is_integer(Id) ->
    (Id + 1) band ?ID_MAX.

to_id(Id) when ?IS_ID(Id) ->
    Id;
to_id(_Other) ->
    null.

max_extra_nonce(1) -> ?NONCE_BYTES_1_MAX;
max_extra_nonce(2) -> ?NONCE_BYTES_2_MAX;
max_extra_nonce(3) -> ?NONCE_BYTES_3_MAX;
max_extra_nonce(4) -> ?NONCE_BYTES_4_MAX;
max_extra_nonce(5) -> ?NONCE_BYTES_5_MAX;
max_extra_nonce(6) -> ?NONCE_BYTES_6_MAX;
max_extra_nonce(7) -> ?NONCE_BYTES_7_MAX.
