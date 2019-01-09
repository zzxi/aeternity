-module(aestratum_extra_nonce_cache).

-behaviour(gen_server).

%% API.
-export([start_link/1,
         get/0,
         free/1
        ]).

%% gen_server.
-export([init/1,
         handle_call/3,
         handle_cast/2
        ]).

-record(state, {
          cache,
          max_value
         }).

-define(SERVER, ?MODULE).

%% API.

start_link(MaxValue) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [MaxValue], []).

get() ->
    gen_server:call(?SERVER, get).

free(ExtraNonce) ->
    gen_server:call(?SERVER, {free, ExtraNonce}).

%% Callbacks.

init([MaxValue]) ->
    {ok, #state{cache = sets:new(), max_value = MaxValue}}.

handle_call(get, _From, State) ->
    case handle_get(State) of
        {ok, ExtraNonce, State1} ->
            {reply, ExtraNonce, State1};
        {error, Rsn, State1} ->
            {reply, Rsn, State1}
    end;
handle_call({free, ExtraNonce}, _From, State) ->
    {ok, State1} = handle_free(ExtraNonce, State),
    {reply, ok, State1}.

handle_cast(_Req, State) ->
    {noreply, State}.

%% Internal functions.

handle_get(#state{cache = Cache, max_value = MaxValue} = State) ->
    case find_extra_nonce(Cache, MaxValue, 50) of
        {ok, ExtraNonce} ->
            State1 = State#state{cache = sets:add_element(ExtraNonce, Cache)},
            {ok, ExtraNonce, State1};
        {error, Rsn} ->
            {error, Rsn, State}
    end.

handle_free(ExtraNonce, #state{cache = Cache} = State) ->
    {ok, State#state{cache = sets:del_element(ExtraNonce, Cache)}}.

%% MaxValue is the max value for the extra nonce. It's selected randomly and
%% checked against the cache if it's already in use until an available nonce
%% is found or Retries is exhausted.
find_extra_nonce(Cache, MaxValue, Retries) when Retries > 0 ->
    %% Random value between 0 and MaxValue.
    ExtraNonce = rand:uniform(MaxValue + 1) - 1,
    case sets:is_element(ExtraNonce, Cache) of
        false -> {ok, ExtraNonce};
        true  -> find_extra_nonce(Cache, MaxValue, Retries - 1)
    end;
find_extra_nonce(_Cache, _MaxValue, 0) ->
    {error, extra_nonce_not_found}.

