%%%-------------------------------------------------------------------
%% @doc sipgraph public API
%% @end
%%%-------------------------------------------------------------------

-module(sipgraph_app).

-behaviour(application).

-export([start/2, stop/1]).

start(_StartType, _StartArgs) ->
    sipgraph_sup:start_link().

stop(_State) ->
    ok.

%% internal functions
