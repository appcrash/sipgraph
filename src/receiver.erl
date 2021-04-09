-module(receiver).

-export([start_link/0]).

-define(UDP_LISTEN_OPTIONS, [binary,{reuseaddr,true}, {active, true}, {recbuf,65535}, {sndbuf, 65535}]).

-define(SOL_SOCKET, 1).
-define(SO_REUSEPORT, 15).

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
      Option = ?UDP_LISTEN_OPTIONS ++ [{ip, Address}];
    {error,_} ->
      logger:error("proxy udp receiver: wrong listen address, use default(listen to all address)"),
      Option = ?UDP_LISTEN_OPTIONS
  end,

  ReuseOption = reuse_port(Option),

  case gen_udp:open(Port,ReuseOption) of
    {ok, _} ->
      loop();
    _ -> logger:error("udp proxy receiver bind address failed")
  end.

%% enable reuse_port can loadbalance on one udp port with multiple processes
reuse_port(Option) ->
  case os:type() of
    {unix,linux} ->
      %% linux,SOL_SOCKET:1, SO_REUSEPORT:15
      [{raw,1,15,<<1:32/native>>} | Option];
    {unix,_} ->
      %% for bsd/darwin, SOL_SOCKET:0xffff, SO_REUSEPORT:0x00000200
      [{raw,16#ffff,16#0200,<<1:32/native>>} | Option];
    _ -> Option
  end.

loop() ->
  receive
    {udp,_Socket,_Ip,_Port,Packet} ->
      packet:handle(Packet);
    R ->
      logger:error("receive udp error ~p",[R])
  end,
  loop().
