-module(aestratum_client_session).

-export([new/0,
         handle_event/2
        ]).

-export([state/1]).

-record(state, {
          phase,
          req_id = 0,
          last_req,
          timer,
          retries = 0
        }).

-define(MAX_RETRIES, application:get_env(aestratum, max_retries, 3)).
-define(MSG_TIMEOUT, application:get_env(aestratum, timeout, 30000)).

%% API.

new() ->
    #state{phase = connected}.

handle_event({conn, What}, State)  ->
    handle_conn_event(What, State);
handle_event({miner, What}, State) ->
    handle_miner_event(What, State).

state(#state{phase = Phase, req_id = ReqId, last_req = LastReq,
             timer = Timer, retries = Retries}) ->
    #{mode => client, phase => Phase, req_id => ReqId, last_req => LastReq,
      timer_phase => case Timer of {_, TPhase} -> TPhase; _ -> undefined end,
      retries => Retries}.

%% Internal functions.

handle_conn_event(init, State) ->
    send_req(configure, State);
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
        undefined -> ok
    end,
    %% TODO: reason, log
    {stop, State#state{phase = disconnected, timer = undefined}}.

handle_miner_event(_What, State) ->
    %% TODO
    {no_send, State}.

%% Handle received messages.

recv_msg(#{type := rsp, id := Id} = Rsp,
         #state{last_req = #{id := Id, method := Method}} = State) ->
    case aestratum_jsonrpc:validate_rsp(Method, Rsp) of
        {ok, Rsp1}   -> recv_msg1(Rsp1, State);
        {error, Rsn} -> recv_msg_error(Rsn, State)
    end;
recv_msg(#{type := ntf} = _Ntf, State) ->
    {no_send, State};
recv_msg(#{type := req, method := reconnect} = Req, State) ->
    %% TODO
    {no_send, State}.

recv_msg_error(parse_error, State) ->
    {no_send, State};
recv_msg_error({invalid_msg, _MaybeId}, State) ->
    {no_send, State};
recv_msg_error({invalid_method, _MaybeId}, State) ->
    {no_send, State};
recv_msg_error({invalid_param, _MaybeId, _Param}, State) ->
    {no_send, State};
recv_msg_error({internal_error, _MaybeId}, State) ->
    {no_send, State}.

recv_msg1(#{method := configure, result := []},
          #state{timer = Timer} = State) ->
    %% TODO: configure has no params currently
    cancel_timer(Timer),
    send_req(subscribe, State#state{phase = configured, timer = undefined,
                                    retries = 0});
recv_msg1(#{method := subscribe, result := [SessionId, ExtraNonce]},
          #state{timer = Timer} = State) ->
    %% TODO: log successful subscribe
    %% TODO: save SessionId(?) and ExtraNonce
    cancel_timer(Timer),
    send_req(authorize, State#state{phase = subscribed, timer = undefined,
                                    retries = 0});
recv_msg1(#{method := authorize, result := true},
          #state{timer = Timer} = State) ->
    %% TODO: log authorization success
    cancel_timer(Timer),
    {no_send, State#state{phase = authorized, timer = undefined,
                          retries = 0}};
recv_msg1(#{method := authorize, result := false},
          #state{timer = Timer} = State) ->
    %% TODO: log invalid user/password
    cancel_timer(Timer),
    {stop, State#state{phase = disconnected, timer = undefined,
                       retries = 0}};
recv_msg1(#{method := submit, result := true}, State) ->
    %% TODO: log successful submit
    {no_send, State};
recv_msg1(#{method := submit, result := false}, State) ->
    %% TODO: log unsuccessful submit
    {no_send, State};
recv_msg1(#{method := Method, reason := Rsn, msg := ErrMsg,
            data := ErrData}, State) ->
    %% TODO: log error response
    %% TODO: maybe retry
    {no_send, State}.

%% Handle timeout.

handle_timeout(#state{phase = connected, timer = {_TRef, connected}} = State) ->
    %% TODO: also subscribe request is possible here
    send_req(configure, State);
handle_timeout(#state{phase = configured, timer = {_TRef, configured}} = State) ->
    send_req(subscribe, State);
handle_timeout(#state{phase = subscribed, timer = {_TRef, subscribed}} = State) ->
    send_req(authorize, State);
handle_timeout(#state{phase = Phase} = State) when
      (Phase =:= connected) or (Phase =:= configured) or (Phase =:= subscribed) ->
    {no_send, State}.

%% Client to server requests.

send_req(ReqType, #state{retries = Retries} = State) ->
    case Retries > ?MAX_RETRIES of
        true ->
            {stop, State#state{phase = disconnected, timer = undefined}};
        false ->
            State1 = State#state{retries = Retries + 1},
            case ReqType of
                configure -> send_configure_req(State1);
                subscribe -> send_subscribe_req(State1);
                authorize -> send_authorize_req(State1);
                submit    -> send_submit_req(State1)
            end
    end.

send_configure_req(#state{phase = Phase, req_id = Id} = State) ->
    ReqMap = #{type => req, method => configure, id => Id, params => []},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap,
                            timer = set_timer(Phase)}}.

send_subscribe_req(#state{phase = Phase, req_id = Id} = State) ->
    UserAgent = <<"ae/0.0.1">>,
    Host = <<"localhost">>,
    Port = 9999,
    ReqMap = #{type => req, method => subscribe, id => Id,
               user_agent => UserAgent, session_id => null, host => Host,
               port => Port},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap,
                            timer = set_timer(Phase)}}.

send_authorize_req(#state{phase = Phase, req_id = Id} = State) ->
    User = <<"ae_user">>,
    Password = binary:copy(<<"0">>, 64),
    ReqMap = #{type => req, method => authorize, id => Id,
               user => User, password => Password},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap,
                            timer = set_timer(Phase)}}.

send_submit_req(#state{req_id = Id} = State) ->
    User = <<"ae_user">>,
    JobId = <<"0123456789abcdef">>,
    MinerNonce = <<"012356789">>,
    Pow = lists:seq(1, 42),
    ReqMap = #{type => req, method => submit, id => Id,
               user => User, job_id => JobId, miner_nonce => MinerNonce,
               pow => Pow},
    {ok, Req} = aestratum_jsonrpc:encode(ReqMap),
    {send, Req, State#state{req_id = next_id(Id), last_req = ReqMap}}.

%% Helper functions.

set_timer(Phase) ->
    TRef = erlang:send_after(?MSG_TIMEOUT, self(), timeout),
    {TRef, Phase}.

cancel_timer({TRef, _Phase}) when is_reference(TRef) ->
    erlang:cancel_timer(TRef).

next_id(Id) ->
    aestratum_utils:next_id(Id).

