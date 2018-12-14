-module(aestratum_client_session_tests).

-include_lib("eunit/include/eunit.hrl").

-define(TEST_MODULE, aestratum_client_session).
-define(JSONRPC_MODULE, aestratum_jsonrpc).

session_test_() ->
    {setup,
     fun() -> ok = application:ensure_started(jsx) end,
     fun(_) -> ok = application:stop(jsx) end,
      [{generator, fun client_session/0}]}.

client_session() ->
    {foreach,
     fun() ->
             {ok, Pid} =
                aestratum_dummy_handler:start_link(?TEST_MODULE),
             application:set_env(aestratum, max_retries, 1),
             Pid
     end,
     fun(Pid) ->
             aestratum_dummy_handler:stop(Pid)
     end,
     [fun(Pid) -> t(Pid, init()) end,
      fun(Pid) -> t(Pid, configure_timeout()) end,
      fun(Pid) -> t(Pid, subscribe_timeout()) end,
      fun(Pid) -> t(Pid, authorize_timeout()) end
      % TODO: handle error responses
     ]}.

%% T - title
%% E - event
%% A - action
%% S - session state
%% R - result
t(Pid, Data) ->
    [begin
         R1 = result(Pid, R, aestratum_dummy_handler:handle_event(Pid, event(E))),
         {T, ?_assertEqual(R, R1)}
     end || {T, E, R} <- Data].

event({conn, D}) when is_map(D) ->
    {ok, D1} = ?JSONRPC_MODULE:encode(D),
    {conn, D1};
event(Other) ->
    Other.

result(Pid, {_A, S}, {A1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {A1, S1M};
result(Pid, {_A, S}, {A1, D1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {ok, D1M} = ?JSONRPC_MODULE:decode(D1),
    {A1, D1M, S1M};
result(Pid, {_A, _D, S}, {A1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {A1, S1M};
result(Pid, {_A, D, S}, {A1, D1, S1}) ->
    Ks = maps:keys(S),
    S1M = maps:with(Ks, aestratum_dummy_handler:state_to_map(Pid, S1)),
    {ok, D1M0} = ?JSONRPC_MODULE:decode(D1),
    D1M = maps:with(maps:keys(D), maybe_rsp_result(D, D1M0)),
    {A1, D1M, S1M}.

%% If type is rsp, we need to validate the result.
maybe_rsp_result(#{type := rsp, method := M}, #{type := rsp} = D1M0) ->
    {ok, D1M} = ?JSONRPC_MODULE:validate_rsp(M, D1M0),
    D1M;
maybe_rsp_result(_D, D1M0) ->
    D1M0.

init() ->
    T = <<"init - client">>,
    L =
        [{{conn, init},
          {send,
           #{type => req, method => configure},
           #{mode => client, phase => connected, timer_phase => connected}}
         }],
    [{T, E, R} || {E, R} <- L].

configure_timeout() ->
    T = <<"configure timeout - client">>,
    L =
        [{{conn, init},
          {send,
           #{type => req, method => configure},
           #{mode => client, phase => connected, timer_phase => connected}}
         },
         {{conn, timeout},
          {send,
           #{type => req, method => configure},
           #{mode => client, phase => connected, timer_phase => connected}}
         },
         {{conn, timeout},
          {stop,
           #{mode => client, phase => disconnected, timer_phase => undefined}}
         }],
    [{T, E, R} || {E, R} <- L].

subscribe_timeout() ->
    T = <<"subscribe timeout - client">>,
    L =
        [{{conn, init},
          {send,
           #{type => req, method => configure, id => 0},
           #{mode => client, phase => connected, timer_phase => connected}}
         },
         {{conn, #{type => rsp, method => configure, id => 0, result => []}},
          {send,
           #{type => req, method => subscribe},
           #{mode => client, phase => configured, timer_phase => configured}}
         },
         {{conn, timeout},
          {send,
           #{type => req, method => subscribe},
           #{mode => client, phase => configured, timer_phase => configured}}
         },
         {{conn, timeout},
          {stop,
           #{mode => client, phase => disconnected, timer_phase => undefined}}
         }],
    [{T, E, R} || {E, R} <- L].

authorize_timeout() ->
    T = <<"authorize timeout - client">>,
    L =
        [{{conn, init},
          {send,
           #{type => req, method => configure, id => 0},
           #{mode => client, phase => connected, timer_phase => connected}}
         },
         {{conn, #{type => rsp, method => configure, id => 0, result => []}},
          {send,
           #{type => req, method => subscribe, id => 1},
           #{mode => client, phase => configured, timer_phase => configured}}
         },
         {{conn, #{type => rsp, method => subscribe, id => 1,
                   result => [null, <<"0123456789">>]}},
          {send,
           #{type => req, method => authorize},
           #{mode => client, phase => subscribed, timer_phase => subscribed}}
          },
         {{conn, timeout},
          {send,
           #{type => req, method => authorize},
           #{mode => client, phase => subscribed, timer_phase => subscribed}}
         },
         {{conn, timeout},
          {stop,
           #{mode => client, phase => disconnected, timer_phase => undefined}}
         }],
    [{T, E, R} || {E, R} <- L].

