%%%-------------------------------------------------------------------
%% @doc sipgraph top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(sipgraph_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs = [
    #{
      id => packet,
      start => {packet,start_link,[]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [packet]
    },

    #{
      id => sip_session,
      start => {sip_session,start_link,[]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [sip_session]
    },

    #{
      id => sip_db,
      start => {sip_db,start_link,[]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [sip_db]
    },

    #{
      id => receiver,
      start => {receiver,start_link,[]},
      restart => permanent,
      shutdown => infinity,
      type => worker,
      modules => [receiver]
     }

    ],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
