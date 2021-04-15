-module(metric_SUITE).
-compile(export_all).
-include_lib("common_test/include/ct.hrl").

all() ->
  [metric_test].


init_per_testcase(_Case,Config) ->
  process_flag(trap_exit,true),
  metric:start_link(),
  Config.

end_per_testcase(_Case,_Config) ->
  ok.

metric_test(_Config) ->
  metric:count(key1),
  #{key1 := 1} = metric:info(),
  metric:count(key1),
  #{key1 := 2} = metric:info(),
  metric:count(key2),
  #{key1 := 2,key2 := 1} = metric:info(),
  ok.
