%%%-------------------------------------------------------------------
%%% @author Happi (Erik Stenman)
%%% @copyright (C) 2017, Aeternity Anstalt
%%% @doc
%%%     Implementation of the Recursive Length Prefix.
%%%
%%%     Part of the documentation is quoted from the Etherium
%%%     Yellow Paper (c) DR. GAVIN WOOD
%%%
%%% This is a serialisation method for encoding arbitrarily structured
%%% binary data (byte arrays).
%%%
%%%  We define the set of possible structures
%%%    S:  S ≡ L ∪ B 
%%%        L ≡ {t : t = (t[0], t[1], ...) ∧ ∀n<|t| t[n] ∈ T}
%%%        B ≡ {b : b = (b[0], b[1], ...) ∧ ∀n<|b| b[n] ∈ O}
%%%  Where O is the set of bytes.
%%%
%%%  Thus B is the set of all sequences of bytes (otherwise
%%%  known as byte-arrays, and a leaf if imagined as a tree),
%%%  L is the set of all tree-like (sub-)structures that are not a
%%%  single leaf (a branch node if imagined as a tree) and
%%%  T is the set of all byte-arrays and such structural sequences.
%%%
%%% The dot operator ·  performs sequence concatenation.
%%%      (a) · (b, c) · (d, e) = (a, b, c, d, e)
%%%     <<A>> · <<B, C>> · <<D, E>> = <<A, B, C, D, E>>
%%%    or
%%%     A · B · C = <<A/binary, B/binary, C/binary>>
%%%
%%%
%%% @end
%%% Created : 16 Oct 2017
%%%-------------------------------------------------------------------
%% Recursive Length Prefix
-module(aevm_rlp).
-export([rlp/1]).


%%
%%  We define the RLP function as RLP through two sub-functions, the
%%  first handling the instance when the value is a byte array, the
%%  second when it is a sequence of further values:
%%  RLP(x) ≡ Rb(x) if x ∈ B
%%           Rl(x) otherwise
rlp(X) when is_binary(X) -> rb(X);
rlp(X)                   -> rl(X).
    
%%  If the value to be serialised is a byte-array, the RLP
%%  serialisation takes one of three forms:
%%    • If the byte-array contains solely a single byte and that
%%      single byte is less than 128, then the input is exactly
%%       equal to the output.
%%    • If the byte-array contains fewer than 56 bytes,
%%      then the output is equal to the input prefixed by the byte
%%      equal to the length of the byte array plus 128.
%%    • Otherwise, the output is equal to the input prefixed by the
%%      minimal-length byte-array which when interpreted as a big-endian
%%      integer is equal to the length of the input byte array,
%%      which is itself prefixed by the number of bytes required to
%%      faithfully encode this length value plus 183. 
%%
%%  Formally, we define
%%                x                               if |x| = 1 ∧ x[0] < 128
%%  Rb: Rb(x) ≡   (128 + |x|) · x                 else if |x| < 56
%%                (183 + |BE(|x|)|) · BE(|x|) · x otherwise
rb(<<B>> = X) when B < 128 -> X; 
rb(X) when byte_size(X) < 56 -> 
    Size = byte_size(X),
    FirstByte = 128 + Size,
    <<FirstByte, X/binary>>;
rb(X) ->
    Size = byte_size(X),
    BE = be(Size),
    BESize = byte_size(BE),
    FirstByte = 183 + BESize,
    <<FirstByte, BE/binary, X/binary>>.
				  
%% 
%%                                             n<|b|
%%      BE(x) ≡ (b0, b1, ...) : b0 =/= 0 ∧ x =   Σ    bn · 256^(|b|−1−n)
%%                                              n=0
%%
%% Thus BE is the function that expands a positive integer value to a
%% big-endian byte array of minimal length
be(X) -> binary:encode_unsigned(X).

%% If instead, the value to be
%% serialised is a sequence of other items then the RLP serialisation
%% takes one of two forms:
%%    • If the concatenated serialisations of each contained item is
%%      less than 56 bytes in length, then the output is equal to that
%%      concatenation prefixed by the byte equal to the length of this
%%      byte array plus 192.
%%    • Otherwise, the output is equal to the concatenated serialisations
%%      prefixed by the minimal-length byte-array which when interpreted
%%      as a big-endian integer is equal to the length of the concatenated
%%      serialisations byte array, which is itself prefixed by the number
%%      of bytes required to faithfully encode this length value plus 247.
%% 
%% Thus we finish by formally defining
%%   Rl: Rl(x) ≡ ( (192 + |s(x)|) · s(x) if |s(x)| < 56
%%                  247 + BE(|s(x)|) · BE(|s(x)|) · s(x) otherwise
%%
%%       s(x) ≡ RLP(x0) · RLP(x1)...
%%
%% If RLP is used to encode a scalar, defined only as a positive
%% integer (P or any x for Px), it must be specified as the shortest
%% byte array such that the big-endian interpretation of it is
%% equal. Thus the RLP of some positive integer i is defined as:
%%
%%   RLP(i : i ∈ P) ≡ RLP(BE(i))
%%
%% When interpreting RLP data, if an expected fragment is decoded as a
%% scalar and leading zeroes are found in the byte sequence, clients
%% are required to consider it non-canonical and treat it in the same
%% manner as otherwise invalid RLP data, dismissing it completely.
%% There is no specific canonical encoding format for signed or
%% floating-point values.
rl(X) -> X. %% TODO implement this.
    
