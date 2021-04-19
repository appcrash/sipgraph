-module(receiver_sup).
-behaviour(supervisor).
-export([init/1,start_link/0]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

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
	       intensity => 20,
	       period => 1},
  Num = application:get_env(sipgraph,udp_receiver_num,1),
  ChildSpecs = create_child_spec([],Num),
  {ok, {SupFlags, ChildSpecs}}.


create_child_spec(L,0) ->
  lists:reverse(L);
create_child_spec(L,Num) ->
  H = #{
	id => io_lib:format("udp_receiver_~B",[Num]),
	start => {receiver,start_link,[]},
	restart => permanent,
	shutdown => infinity,
	type => worker,
	modules => []
       },
  create_child_spec([H | L],Num - 1).
