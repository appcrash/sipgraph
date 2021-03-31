-module(receiver).

-export([start_link/0]).

-define(UDP_LISTEN_OPTIONS, [binary, {active, true}, {recbuf,65535}, {sndbuf, 65535}]).

%% simple udp receiving loop which is supervised
%% receive as quickly as process can to avoid socket buffer running out
start_link() ->
  Pid = spawn_link(fun() -> receive_udp() end),
  {ok,Pid}.


receive_udp() ->
  {ok,Ip} = application:get_env(sip_ip),
  {ok,Port} = application:get_env(sip_port),

  case inet:parse_address(Ip) of
    {ok,Address} ->
      UdpOption = ?UDP_LISTEN_OPTIONS ++ [{ip, Address}];
    {error,_} ->
      logger:error("proxy udp receiver: wrong listen address, use default(listen to all address)"),
      UdpOption = ?UDP_LISTEN_OPTIONS
  end,

  case gen_udp:open(Port,UdpOption) of
    {ok, _} ->
      loop();
    _ -> logger:error("udp proxy receiver bind address failed")
  end.


loop() ->
  receive
    {udp,_Socket,_Ip,_Port,Packet} ->
      packet:handle(Packet);
    R ->
      logger:error("receive udp error ~p",[R])
  end,
  loop().
