-module(aelog_app).

-behaviour(application).

-export([check_env/0]).

%% Application callbacks
-export([start/2,
         stop/1]).

-define(LOGGING_HWM_CFG_KEY, [<<"logging">>, <<"hwm">>]).
-define(LOGGING_LEVEL_CFG_KEY, [<<"logging">>, <<"level">>]).

-define(LOG_FILE_DIR, "log").
-define(LOG_FILE_PREFIX, "epoch").

-define(DEFAULT_LAGER_SINK, lager_event).
-define(LAGER_FILE_BACKEND, lager_file_backend).

%%====================================================================
%% API
%%====================================================================

%% Checking user-provided configs. The logic is somewhat complicated
%% by the fact that 'setup' is not guaranteed to start before lager,
%% so we have to be prepared to apply changes to both the lager env
%% and the (possibly) running lager. (This problem is solvable, but not
%% trivially. Basically, the aeternity.rel file must be pre-sorted and passed
%% to relx.
%% Run as setup hook. At this stage, lager is setup with console only - no files.
check_env() ->
    check_env([{?LOGGING_HWM_CFG_KEY, fun set_hwm/1},
               {?LOGGING_LEVEL_CFG_KEY, fun check_level/1}]).

start(_StartType, _StartArgs) ->
    %% Lager is a dependency of this application so lager is
    %% guaranteed to be running.
    %%
    %% All sinks are active thanks to lager having read its portion of
    %% system config.  Though only console backends are active - no
    %% file backends yet.
    %%
    %% User configuration (containing items relevant for logging on
    %% file) is loaded and lager is started: lager file handlers can
    %% now be started.
    ok = start_lager_file_handlers(),
    %% All lager backends are active - both console and file.
    ok = lager:info("Logging setup completed"),
    aelog_sup:start_link().


stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

check_env(Spec) ->
    lists:foreach(
      fun({K, F}) ->
              case aeu_env:user_config(K) of
                  undefined -> ignore;
                  {ok, V}   -> set_env(F, V)
              end
      end, Spec).

set_env(F, V) when is_function(F, 1) ->
    F(V).

start_lager_file_handlers() ->
    Sinks = lager:list_all_sinks(), %% All sinks are active.
    %% Hardcode expectation that default sink is active.
    {true, _} = {lists:member(?DEFAULT_LAGER_SINK, Sinks), Sinks},
    %% Hardcode expectations that base system config:
    %% * Has all active sinks.
    %% * Has file handler for each sink.
    FileHandlersBaseSysCfg = lists:map(fun(S) -> {S, get_file_handler_for_sink(S)} end, Sinks),
    FileHandlersCfg1 =
        case aeu_env:user_config(?LOGGING_HWM_CFG_KEY) of
            undefined -> FileHandlersBaseSysCfg;
            {ok, Hwm} ->
                lists:map(fun({S, SCfg}) -> {S, overridden_file_handler_config(SCfg, high_water_mark, Hwm)} end, FileHandlersBaseSysCfg)
        end,
    FileHandlersCfg2 =
        case aeu_env:user_config(?LOGGING_LEVEL_CFG_KEY) of
            undefined -> FileHandlersCfg1;
            {ok, Level} ->
                %% Level already validated by `set_level`.
                lists:map(fun({S, SCfg}) -> {S, case S of epoch_metrics_lager_event -> SCfg; _ -> overridden_file_handler_config(SCfg, level, Level) end} end, FileHandlersCfg1)
        end,
    %% Guarantee that the file in each file handler is distinct, by using distinct name suffixes.
    FileHandlersCfg3 = lists:map(fun({S, SCfg}) -> {S, overridden_file_handler_config(SCfg, file, file_for_sink(S))} end, FileHandlersCfg2),
    lists:foreach(fun({S, SCfg}) -> lager_app:start_handler(S, ?LAGER_FILE_BACKEND, SCfg) end, FileHandlersCfg3),
    ok.

get_file_handler_for_sink(Sink) ->
    {ok, Cfg} = aeu_env:get_env(aecore, [lager_file_handlers, sinks, Sink, handlers, ?LAGER_FILE_BACKEND]),
    Cfg.

overridden_file_handler_config(BaseCfg, K, V) ->
    lists:keystore(K, 1, BaseCfg, {K, V}).

file_for_sink(S) ->
    filename:join(?LOG_FILE_DIR, file_for_sink(S, ?LOG_FILE_PREFIX) ++ ".log").

file_for_sink(?DEFAULT_LAGER_SINK, Prefix) ->
    Prefix;
file_for_sink(S, Prefix) ->
    Prefix ++ "_" ++ file_sink_suffix(S).

file_sink_suffix(epoch_mining_lager_event) -> "mining";
file_sink_suffix(epoch_pow_cuckoo_lager_event) -> "pow_cuckoo";
file_sink_suffix(epoch_sync_lager_event) -> "sync";
file_sink_suffix(epoch_metrics_lager_event) -> "metrics".
