-module(aemon_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).


-define(SERVER, ?MODULE).
-define(CHILD(Mod,N,Type), {Mod,{Mod,start_link,[]},permanent,N,Type,[Mod]}).


start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    ChildSpecs = [
        ?CHILD(aemon_publisher, 5000, worker)
    ],

    {ok, {{one_for_one, 5, 10}, ChildSpecs}}.
