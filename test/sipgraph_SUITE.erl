-module(sipgraph_SUITE).
-compile([export_all,nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
  [packet_test].


init_per_suite(Config) ->
  {ok,Db} = application:get_env(sipgraph,sip_session_db),
  os:cmd("rm -rf " ++ Db),
  application:ensure_all_started(sipgraph,temporary),
  Config.

end_per_suite(_Config) ->
  application:stop(sipgraph),
  ok.

init_per_testcase(_Case,Config) ->
  Config.

end_per_testcase(_Case,_Config) ->
  ok.

packet_test(_Config) ->
  ok.
