-module(aemon_publisher).
-behaviour(gen_server).
-compile([export_all]).

-export([tick/0]).


-export([start_link/0]).

-export([
          init/1
        , handle_call/3
        , handle_cast/2
        , handle_info/2
        , terminate/2
        , code_change/3
        ]).

-record(state, {pubkey = <<>>}).

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init(_) ->
    self() ! tick,
    {ok, #state{}}.

terminate(_Reason, _St) ->
    ok.

handle_call(_Req, _From, St) ->
    {reply, {error, unknown_request}, St}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(tick, St) ->
    tick(),
    timer:send_after(interval(), tick),
    {noreply, St};
handle_info(_Msg, St) ->
    {noreply, St}.

code_change(_FromVsn, St, _Extra) ->
    {ok, St}.

%%

tx_group_size() -> 10.

interval() ->
    10*1000.

pubkey() -> <<>>.

privkey() -> <<>>.

amount() -> 20000.
fee() -> 18000.

signed_tx(Height, Nonce, Payload) ->
    PrivKey = privkey(),
    Account = aec_id:create(account, pubkey()),
    Amount = amount(),
    Fee = fee(),

    {ok, Tx0} = aec_spend_tx:new(
                  #{ sender_id    => Account,
                     recipient_id => Account,
                     amount       => Amount,
                     nonce        => Nonce,
                     ttl          => Height + 5,
                     payload      => Payload,
                     fee          => Fee}
                 ),
    {ok, Tx} = aec_spend_tx:new(
                 #{ sender_id    => Account,
                    recipient_id => Account,
                    amount       => Amount,
                    nonce        => Nonce,
                    ttl          => Height + 5,
                    payload      => Payload,
                    fee          => aetx:min_fee(Tx0, Height)}
                ),
    BinForNetwork = aec_governance:add_network_id( aetx:serialize_to_binary(Tx) ),
    aetx_sign:new(Tx, [enacl:sign_detached(BinForNetwork, PrivKey)]).

tx_pool_push(Tx) ->
    aec_tx_pool:push(Tx).

to_hash(Block) ->
    EncType = case aec_blocks:type(Block) of
                  micro -> micro_block_hash;
                  key -> key_block_hash
              end,
    {ok, Hash} = aec_blocks:hash_internal_representation(Block),
    aehttp_api_encoder:encode(EncType, Hash).

to_payload(Height, KeyH, TopH, {M1, M2, M3}, N) ->
    IO = io_lib:format("~p:~s-~s:~p-~p-~p:~p", [Height, KeyH, TopH, M1, M2, M3, N]),
    iolist_to_binary(IO).

tick() ->
    Timestamp = os:timestamp(),
    PubKey = pubkey(),

    {ok, BaseNonce} = case aec_next_nonce:pick_for_account(PubKey) of
        {error, _} -> {ok, 0};
        OK -> OK
    end,

    Top = aec_chain:top_block(),
    {ok, TopKey} = aec_chain:top_key_block(),
    Height = aec_blocks:height(Top),

    HashT  = to_hash(Top),
    HashTK = to_hash(TopKey),

    [ begin
          Payload = to_payload(Height, HashTK, HashT, Timestamp, N),
          Tx = signed_tx(Height, BaseNonce+N-1, Payload),
          tx_pool_push(Tx)
      end || N <- lists:seq(1, tx_group_size()) ].
