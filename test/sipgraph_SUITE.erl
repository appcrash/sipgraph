-module(sipgraph_SUITE).
-compile([export_all,nowarn_export_all]).
-include_lib("common_test/include/ct.hrl").

all() ->
  [packet_test].


init_per_suite(Config) ->
  {ok,Db} = application:get_env(sipgraph,sip_session_db),
  os:cmd("rm -rf " ++ Db),
  application:ensure_all_started(inet,temporary),
  application:ensure_all_started(sipgraph,temporary),
  Config.

end_per_suite(_Config) ->
  application:stop(sipgraph),
  application:stop(inet),
  ok.

init_per_testcase(_Case,Config) ->
  {ok,Socket} = gen_udp:open(6666,[{reuseaddr,true},{active,false}]),
  [{udp_socket,Socket} | Config].

end_per_testcase(_Case,Config) ->
  Socket = ?config(udp_socket,Config),
  gen_udp:close(Socket),
  ok.

packet_test(Config) ->
  P = get_test_path(Config,"invite.dtl"),
  erlydtl:compile(P,invite),
  {ok,D} = invite:render(#{sid => "hhh",from => "123", to =>"321"}),
  Socket = ?config(udp_socket,Config),
  send_udp(Socket,D),
  [#{<<"session">> := Session,<<"signal">> := Signal}] =
    jsone:decode(list_to_binary(http_sip_req(session,"hhh")),[{keys,binary}]),
  #{<<"session_id">> := <<"hhh">>
   ,<<"caller">> := <<"123">>,
   <<"callee">> := <<"321">>} = Session,
  [_,Origin] = binary:split(iolist_to_binary(D),<<"\r\n">>),
  [#{<<"seq">> := 1,<<"packet">> := Origin}] = Signal,

  #{<<"packet_received">> := 1,<<"session_created">> := 1} =
    jsone:decode(list_to_binary(http_metric_req()),[{keys,binary}]),
  ok.

http_metric_req() ->
  {ok,Host} = application:get_env(sipgraph,http_ip),
  {ok,Port} = application:get_env(sipgraph,http_port),
  Url = io_lib:format("http://~s:~B/metric",[Host,Port]),
  {ok,{_StatusLine,_Headers,Body}} = httpc:request(Url),
  Body.

http_sip_req(Which,Id) ->
  {ok,Host} = application:get_env(sipgraph,http_ip),
  {ok,Port} = application:get_env(sipgraph,http_port),
  Action = case Which of
	     session -> "get_session";
	     caller -> "get_caller";
	     callee -> "get_callee"
	   end,
  Url = io_lib:format("http://~s:~B/sip/~s?id=~s",[Host,Port,Action,Id]),
  {ok,{_StatusLine,_Headers,Body}} = httpc:request(Url),
  Body.

send_udp(Socket,Packet) ->
  {ok,Ip} = application:get_env(sipgraph,sip_ip),
  {ok,Port} = application:get_env(sipgraph,sip_port),
  {ok,Ipv4} = inet:parse_ipv4_address(Ip),
  gen_udp:send(Socket,Ipv4,Port,Packet).

get_test_path(Config,Filename) ->
  DataDir = ?config(data_dir,Config),
  filename:join([DataDir,Filename]).

get_test_data(Config,Filename) ->
  {ok,Data} = file:read_file(get_test_path(Config,Filename)),
  Data.
