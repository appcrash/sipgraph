-module(sip_session).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,handle_info/2,start_link/0]).
-export([cast/1,query/4]).


-include("common.hrl").

%% this module analyze packet and record INVITE signalling info
%% only the initial INVITE is written to mnesia database, i.e. re-INVITE
%% would be passed.
%% also provide query function: by session_id | caller num | callee num


% cmd pattern, example: INVITE sip:+123456789@hb.ims.3gppnetwork.org SIP/2.0
-define(RE_REQUEST_COMMAND,"(?i)^([\\w]+)\\s+.+\\s+SIP\/2\.0\\s*$").
% pattern: Header: Value
-define(RE_REQUEST_HEADER,"^([-\\w]+)\:\s*(.*)$").
% extract caller/callee info from  From/To value
-define(RE_EXTRACT_PHONE,"(?i)[^<]*<\\s*(?:tel|sip):\\+?(?:86)?(\\d+).*").
% extract session-id line
-define(RE_SESSION_ID,"(?i)^session-id:\\s*(\\S+)\\s*$").


-type analyze_result() :: invalid | {ok, map()}.

init(State) ->
  %% session_info table, record:
  %% {session_id :: string(), seq :: integer(), lastest_timestamp :: integer()}
  ets:new(session_info,[named_table,set]),

  ets:new(session_regex,[named_table,set]),
  lists:foreach(
    fun({Name,Pattern}) ->
	{ok,MP} = re:compile(Pattern),
	ets:insert(session_regex,{Name,MP})
    end,[{command,?RE_REQUEST_COMMAND},{header,?RE_REQUEST_HEADER},
	 {phone,?RE_EXTRACT_PHONE},{session_id,?RE_SESSION_ID}]),
  {ok,State}.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call({query,{F,V,Start,End}},_From,State) ->
  R = case query_by_field(F,V,Start,End) of
	not_found -> [];
	SessionList -> SessionList
      end,
  {reply,R,State};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({packet,Packet},State) ->
  metric:count(packet_received),
  case analyze(Packet) of
    {ok,M} ->
      #{id := Sid,cmd := Cmd,origin := OriginPacket} = M,
      case update_session_info(Sid) of
	{exist,Seq} ->
	  sip_db:store(Sid,Seq,OriginPacket);
	new_session ->
	  case Cmd of
	    "invite" ->
	      logger:info("record session: ~p",[Sid]),
	      SR = map_to_session(M),
	      mnesia:dirty_write(SR),
	      sip_db:store(Sid,1,OriginPacket),
	      metric:count(session_created);
	    _ ->
	      logger:error("first packet of session(~p) with cmd ~p",[Sid,Cmd])
	  end
	end;
    invalid ->
      logger:error("invalid packet, head of packet: ~p",[binary:part(Packet,{0,100})]),
      do_nothing
  end,
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.

%% handle session
%% create new record in ets, start expiration timer if it's new session
%% update sequence_counter/timestamp if it already exists
-spec update_session_info(string()) -> new_session | {exist,Seq :: integer()}.
update_session_info(Sid) ->
  Ts = os:system_time(millisecond),
  case ets:lookup(session_info,Sid) of
    [{_,Seq,_}] ->
      %% increase seq counter by 1 and update timestamp
      ets:update_element(session_info,Sid,[{2,Seq + 1},{3,Ts}]),
      {exist,Seq + 1};
    _ ->
      %% first time session seen, track it as new session, counter starts at 1
      %% also start the timer to expire the session or the ets would run out of memory
      ets:insert(session_info,{Sid,1,Ts}),
      do_expire_timer(Sid,2 * ?SESSION_EXPIRE_TIME,Ts),
      new_session
  end.



%% ##################### API ###########################
cast(Packet) ->
  gen_server:cast(?MODULE,{packet,Packet}).

-spec query(atom(),string(),integer(),integer()) -> [#session{}].
query(Field,Value,TsStart,TsEnd) ->
  gen_server:call(?MODULE,{query,{Field,Value,TsStart,TsEnd}}).

%% #################### API #############################

-spec map_to_session(map()) -> #session{}.
map_to_session(M) ->
  #{id := Id,caller := Caller,callee := Callee} = M,
  #session{id = Id,caller=Caller,callee=Callee,
	   timestamp = os:system_time(millisecond)}.

-spec query_by_field(atom(),string(),integer(),integer()) -> [#session{}] | not_found.
query_by_field(ByField,Value,TsStart,TsEnd) ->
  MatchHead = case ByField of
  		id -> #session{id='$1',timestamp='$2',_='_'};
  		caller -> #session{caller='$1',timestamp='$2',_='_'};
  		callee -> #session{callee='$1',timestamp='$2',_='_'}
  	      end,
  MatchSpec =
    [
     {MatchHead,
      % Guard:    (id == value) && (start < timestamp < end)
      [
       {'and',
      	{'=:=','$1',Value},
      	{'and',
      	 {'=<',TsStart,'$2'},
      	 {'=<','$2',TsEnd}
      	}
       }
      ],
      % Body, whole record here
      ['$_']
     }
    ],
  mnesia:dirty_select(session,MatchSpec).


%% search the packet binary with
%% request cmd name
%% session id
%% from with tag
%% to without tag
%% and return these info as map:
%% {ok,{cmd => Cmd,id => Sid,caller => Caller,callee => Callee,origin=OriginPacket}}
%% or return invalid if not a valid packet
-spec analyze(binary()) -> analyze_result().
analyze(Packet) ->
  case binary:split(Packet,<<"\r\n">>) of
    [SessionLine , OriginPacket ] ->
      [{_,MP}] = ets:lookup(session_regex,session_id),
      case re:run(SessionLine,MP,[{capture,[1],list}]) of
	{match,[Sid]} ->
	  Lines = binary:split(OriginPacket,<<"\r\n">>,[global]),
	  analyze_header(#{id => Sid,origin => OriginPacket,cmd => "unknown"},Lines);
	_ ->
	  logger:error("invalid packet without session id, first line: ~p",[SessionLine]),
	  invalid
      end;
    _ ->
      logger:error("wrong packet format"),
      invalid
  end.

analyze_header(_,[]) -> invalid;
analyze_header(M,_) when map_size(M) == 5 -> {ok,M};
analyze_header(_,[<<>> | _T]) -> invalid; % all headers checked, here comes empty '\r\n'
analyze_header(#{cmd := "unknown"} = M,[Line|T])  ->
  [{_,MP}] = ets:lookup(session_regex,command),
  M1 = case re:run(Line,MP,[{capture,[1],list}]) of
	 {match,[Cmd]} ->
	   M#{cmd := string:to_lower(Cmd)};
	 _ -> analyze_header(M,Line)
       end,
  analyze_header(M1,T);
analyze_header(M,[Line|T]) ->
  M1 = analyze_header(M,Line),
  analyze_header(M1,T);
analyze_header(M,Line) ->
  [{_,MP}] = ets:lookup(session_regex,header),
  case re:run(Line,MP,[{capture,[1,2],list}]) of
    {match,[Header,Value]} ->
      H = string:to_lower(Header),
      [{_,PMP}] = ets:lookup(session_regex,phone),
      case H of
	"from" ->
	  case re:run(Value,PMP,[{capture,[1],list}]) of
	    {match,[Phone]} ->
	      M#{caller => Phone};
	    _ -> M
	  end;
	"to" ->
	  case re:run(Value,PMP,[{capture,[1],list}]) of
	    {match,[Phone]} ->
	      M#{callee => Phone};
	    _ -> M
	  end;
	_ -> M
      end;
    _ -> M
  end.



%% expire timer schedules 'check_session' after session created
handle_info({check_session,Sid,ExpireTs},State) ->
  case ets:lookup(session_info,Sid) of
    [{_,_,LastestTs}] ->
      Diff = ExpireTs - LastestTs,
      if
	Diff < ?SESSION_EXPIRE_TIME -> 		% the session is active, extend the expiration timestamp
	  do_expire_timer(Sid,2 * ?SESSION_EXPIRE_TIME,ExpireTs);
	true ->
	  logger:info("delete expired session ~p",[Sid]),
	  ets:delete(session_info,Sid)
      end;
    _ -> not_found
  end,
  {noreply,State};
handle_info(_,State) ->
  {noreply,State}.

do_expire_timer(Sid,After,Now) ->
  erlang:send_after(After,self(),{check_session,Sid,After + Now}).
