-module(web_sip_handler).
-export([init/2]).

-include("common.hrl").
%% this handler provider query method:
%% get_session,get_caller,get_callee
%% all of them use the same argument
%% id(MUST): session id | caller num | callee num
%% ts_start(Optional): timestamp begins
%% ts_end(Optional): timestamp ends
init(Req,Opts) ->
    case cowboy_req:binding(operation,Req) of
      <<"get_session">> ->
	sip_get_session_by_field(Req,Opts,id);
      <<"get_caller">> ->
	sip_get_session_by_field(Req,Opts,caller);
      <<"get_callee">> ->
	sip_get_session_by_field(Req,Opts,callee);
      undefined ->
	sip_error(400,Req)
    end.


sip_error(ErrNum,Req) ->
  cowboy_req:reply(ErrNum,Req).

sip_get_session_by_field(Req,Opts,Field) ->
  {ok,Timeout} = application:get_env(sipgraph,http_timeout),
  try
    M = cowboy_req:match_qs([{id,nonempty},{ts_start,int,0},{ts_end,int,0}],Req),
    MM = check_timestamp(M),
    #{id := Id,ts_start := Start,ts_end := End} = MM,
    SessionList = sip_session:query(Field,binary_to_list(Id),Start,End),
    SM = lists:map(
	   fun(#session{id=Sid}=S) ->
	       SignalList = sip_db:read_prefix(Sid,1000,Timeout),
	       #{
		 <<"session">> => session_to_map(S),
		 <<"signal">> => [signal_to_map(Sig) || Sig <- SignalList]
		 }
	   end,SessionList),
    J = jsone:encode(SM),
    Req1 = cowboy_req:reply(200,
			    #{
			      <<"conntent-type">> => <<"application/json,charset=utf-8">>
			    },J,Req),
    {ok,Req1,Opts}
  catch
    error:{badkey,K} ->
      logger:error("bad request with key: ~p",[K]),
      Req2 = sip_error(400,Req),
      {ok,Req2,Opts}
  end.

%% if no timestamp set, set range to 1 day ago
check_timestamp(#{ts_start := S,ts_end := E} = M) ->
  M1 = case S of
	    0 ->
	   maps:update(ts_start,os:system_time(millisecond) - 86400000,M);
	    _ -> M
	  end,
  case E of
    0 ->
      maps:update(ts_end,os:system_time(millisecond),M1);
    _ -> M1
  end.


-spec session_to_map(#session{}) -> map().
session_to_map(S) ->
  #{
    <<"session_id">> => list_to_binary(S#session.id),
    <<"caller">> => list_to_binary(S#session.caller),
    <<"callee">> => list_to_binary(S#session.callee),
    <<"timestamp">> => S#session.timestamp
   }.

-spec signal_to_map(#signal{}) -> map().
signal_to_map(
  #signal{signal_id=#signal_key{seq=Seq},
	 timestamp=Ts,data=Packet}) ->
  #{
    <<"seq">> => Seq,
    <<"timestamp">> => Ts,
    <<"packet">> => Packet
   }.
