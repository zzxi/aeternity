-module(aestratum_server_session).

-export([new/0,
         handle_event/2
        ]).


-ifdef(TEST).
-export([get_host/0,
         get_port/0,
         get_extra_nonce/0,
         state/1]).
-endif.

-record(state, {
          phase,
          timer
        }).

-define(MSG_TIMEOUT, application:get_env(aestratum, timeout, 30000)).

%% API.

new() ->
    #state{phase = connected}.

handle_event({conn, What}, State) ->
    handle_conn_event(What, State);
handle_event({chain, What}, State) ->
    %% TODO: set_target, notify
    handle_chain_event(What, State).

%% Internal functions.

handle_conn_event(init, #state{phase = connected} = State) ->
    {no_send, State#state{timer = set_timer(connected)}};
handle_conn_event(RawMsg, State) when is_binary(RawMsg) ->
    case aestratum_jsonrpc:decode(RawMsg) of
        {ok, Msg}    -> recv_msg(Msg, State);
        {error, Rsn} -> recv_msg_error(Rsn, State)
    end;
%% TODO: {reconnect, Host, Port, WaitTime},...
handle_conn_event(timeout, State) ->
    handle_timeout(State);
handle_conn_event(close, #state{timer = Timer} = State) ->
    case Timer of
        {_TRef, _Phase} -> cancel_timer(Timer);
        undefined       -> ok
    end,
    {stop, State#state{phase = disconnected, timer = undefined}}.

handle_chain_event(_What, State) ->
    %% TODO
    {no_send, State}.

%% Handle received messages.

recv_msg(#{type := req, method := configure} = Req,
         #state{phase = connected} = State) ->
    send_configure_rsp(Req, State);
recv_msg(#{type := req, method := subscribe} = Req,
         #state{phase = connected} = State) ->
    send_subscribe_rsp(Req, State);
recv_msg(#{type := req, method := subscribe} = Req,
         #state{phase = configured} = State) ->
    send_subscribe_rsp(Req, State);
recv_msg(#{type := req, method := authorize} = Req,
         #state{phase = subscribed} = State) ->
    send_authorize_rsp(Req, State);
recv_msg(#{type := req, method := submit} = Req,
         #state{phase = authorized} = State) ->
    send_submit_rsp(Req, State);
recv_msg(#{type := req, method := Method} = Req,
         #state{phase = Phase} = State) when
      ((Method =:= authorize) or (Method =:= submit)) and
      ((Phase =:= connected) or (Phase =:= configured)) ->
    send_not_subscribed_rsp(Req, State);
recv_msg(#{type := req, method := submit} = Req,
         #state{phase = subscribed} = State) ->
    send_unauthorized_worker_rsp(Req, State);
recv_msg(Msg, State) ->
    send_unknown_error_rsp(Msg, State).

%% JSON-RPC error responses.

recv_msg_error(parse_error, State) ->
    RspMap = #{type => rsp, method => undefined, id => null,
               reason => parse_error, data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State};
recv_msg_error({invalid_msg, MaybeId}, State) ->
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_utils:to_id(MaybeId),
               reason => invalid_msg, data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State};
recv_msg_error({invalid_method, MaybeId}, State) ->
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_utils:to_id(MaybeId),
               reason => invalid_method, data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State};
recv_msg_error({invalid_param, Param, MaybeId}, State) ->
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_utils:to_id(MaybeId),
               reason => invalid_param, data => atom_to_binary(Param, utf8)},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State};
recv_msg_error({internal_error, MaybeId}, State) ->
    RspMap = #{type => rsp, method => undefined,
               id => aestratum_utils:to_id(MaybeId),
               reason => internal_error, data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

%% Handle timeout.

handle_timeout(#state{phase = Phase, timer = {_TRef, Phase}} = State) when
      (Phase =:= connected) or (Phase =:= configured) or (Phase =:= subscribed) ->
    %% The timer's phase is the same as the current phase, so the timeout
    %% applies and the connection to the client is closed.
    %% TODO: reason, log
    {stop, State#state{phase = disconnected, timer = undefined}};
handle_timeout(State) ->
    {no_send, State}.

%% Server to client responses.

send_configure_rsp(Req, State) ->
    send_configure_rsp1(validate_configure_req(Req), Req, State).

send_subscribe_rsp(Req, State) ->
    send_subscribe_rsp1(validate_subscribe_req(Req), Req, State).

send_authorize_rsp(Req, State) ->
    send_authorize_rsp1(validate_authorize_req(Req), Req, State).

send_submit_rsp(Req, State) ->
    send_submit_rsp1(validate_submit_req(Req), Req, State).

send_configure_rsp1(ok, #{id := Id}, #state{timer = Timer} = State) ->
    %% TODO: there are no configure params currently
    cancel_timer(Timer),
    RspMap = #{type => rsp, method => configure, id => Id, result => []},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State#state{phase = configured, timer = set_timer(configured)}}.
%%send_configure_rsp1({error, Rsn}, ...

send_subscribe_rsp1(ok, #{id := Id}, #state{timer = Timer} = State) ->
    cancel_timer(Timer),
    %% TODO: session resumption not supported (yet)
    SessionId1 = null,
    %% ?MODULE is present as meck doesn't work well without it.
    ExtraNonce = ?MODULE:get_extra_nonce(),
    RspMap = #{type => rsp, method => subscribe, id => Id,
               result => [SessionId1, ExtraNonce]},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    %% Set timer for authorize request.
    {send, Rsp, State#state{phase = subscribed, timer = set_timer(subscribed)}};
send_subscribe_rsp1({error, Rsn}, #{id := Id}, State) ->
    RspMap = #{type => rsp, method => subscribe, id => Id,
               reason => unknown_error, data => atom_to_binary(Rsn, utf8)},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_authorize_rsp1(ok, #{id := Id}, #state{timer = Timer} = State) ->
    cancel_timer(Timer),
    RspMap = #{type => rsp, method => authorize, id => Id,
               result => true},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    %% No need to set timer after authorization, there are no further
    %% expected requests within a time period. Submit requests do not
    %% require any timeout.
    %% TODO: send set_target immediatelly after authorization (using timeout?)
    {send, Rsp, State#state{phase = authorized, timer = undefined}};
send_authorize_rsp1({error, user_and_password}, #{id := Id}, State) ->
    RspMap = #{type => rsp, method => authorize, id => Id,
               result => false},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_submit_rsp1(ok, #{id := Id, user := User, job_id := JobId,
                       miner_nonce := MinerNonce, pow := Pow}, State) ->
    %% TODO: read from the database based on JobId: BlockVersion, HeaderHash,
    %% Target, ExtraNonce and verify the submitted solution
    %% TODO: if successful, write the solution/share to the db and compute
    %% reward which will be paid later (at least 180 block)
    Submitted = true,
    RspMap = #{type => rsp, method => submit, id => Id, result => Submitted},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.
%%send_submit_rsp1({error, Rsn}, Req, State) ->
%%    Submitted = false, ....

%% Stratum error responses.

send_not_subscribed_rsp(#{id := Id}, State) ->
    RspMap = #{type => rsp, id => Id, reason => not_subscribed,
               data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_unauthorized_worker_rsp(#{id := Id}, State) ->
    RspMap = #{type => rsp, id => Id, reason => unauthorized_worker,
               data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_low_difficulty_share_rsp(#{id := Id}, State) ->
    RspMap = #{type => rsp, id => Id, reason => low_difficulty_share,
               data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_duplicate_share_rsp(#{id := Id}, State) ->
    RspMap = #{type => rsp, id => Id, reason => duplicate_share,
               data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_job_not_found_rsp(#{id := Id}, State) ->
    RspMap = #{type => rsp, id => Id, reason => job_not_found,
               data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

send_unknown_error_rsp(#{id := Id}, State) ->
    RspMap = #{type => rsp, id => Id, reason => unknown_error,
               data => null},
    {ok, Rsp} = aestratum_jsonrpc:encode(RspMap),
    {send, Rsp, State}.

%% TODO: reconnect request.

%% Notifications from server to client.

send_set_target_ntf(_Target, #state{} = State) ->
    %% TODO: compute new target
    {no_send, State}.

send_notify_ntf(_ChainTop, #state{} = State) ->
    {no_send, State}.

%% Helper functions.

set_timer(Phase) ->
    TRef = erlang:send_after(?MSG_TIMEOUT, self(), timeout),
    {TRef, Phase}.

cancel_timer({TRef, _Phase}) when is_reference(TRef) ->
    erlang:cancel_timer(TRef).

validate_configure_req(#{params := []}) ->
    ok.

validate_subscribe_req(Req) ->
    run([fun check_user_agent/1,
         fun check_session_id/1,
         fun check_host/1,
         fun check_port/1], Req).

validate_authorize_req(Req) ->
    run([fun check_user_and_password/1], Req).

validate_submit_req(Req) ->
    run([fun check_job/1], Req).


check_user_agent(#{user_agent := _UserAgent}) ->
    %% Some user agents may not by supported by the server
    ok.

check_session_id(#{session_id := _SessionId}) ->
    ok.

check_host(#{host := Host}) ->
    check_host1(Host, ?MODULE:get_host()).

check_host1(Host, Host) ->
    ok;
check_host1(Host, Host1) ->
    validation_exception({host, Host, Host1}).

check_port(#{port := Port}) ->
    check_port1(Port, ?MODULE:get_port()).

check_port1(Port, Port) ->
    ok;
check_port1(Port, Port1) ->
    validation_exception({port, Port, Port1}).

check_user_and_password(#{user := User, password := Password}) ->
    %% TODO: allow null password?
    %% TODO: user as "public_key.worker"?
    ok. % | {error, user_and_password}

check_job(#{user := User, job_id := JobId, miner_nonce := MinerNonce,
            pow := Pow}) ->
    %% TODO: read from DB and check job id, user,...
    %% TODO: validate solution and target
    ok.

run(Funs, Data) ->
    try
        lists:foreach(fun(Fun) -> Fun(Data) end, Funs),
    catch
        throw:{validation_error, Rsn} ->
            validation_error(Rsn, Data)
    end.

validation_error({Field, Val, ExpVal} = Rsn, #{method := Method} = Data) ->
    lager:warning(
      "Server session error, method: ~p, field: ~p, expected: ~p, got: ~p",
      [Method, Field, ExpVal, Val]),
    {error, Field}.

validation_exception(Rsn) ->
    throw({validation_error, Rsn}).

get_host() ->
    <<"ae.pool.com">>.

get_port() ->
    9999.

get_extra_nonce() ->
    <<"00000001">>.

%% Used for testing only.

state(#state{phase = Phase, timer = Timer}) ->
    #{mode => server, phase => Phase,
      timer_phase => case Timer of {_, TPhase} -> TPhase; _ -> undefined end}.

