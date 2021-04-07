-module(sip_session).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([cast/1]).

% pattern: INVITE sip:+8617871233155@hb.ims.mnc000.mcc460.3gppnetwork.org SIP/2.0
-define(RE_REQUEST_COMMAND,"(?i)^([\\w]+)\\s+.+\\s+SIP\/2\.0\\s*$").
% pattern: Header: Value
-define(RE_REQUEST_HEADER,"^([-\\w]+)\:\s*(.*)$").
% check whether from/to header with tag
-define(RE_HAS_TAG,".*tag=(.+)$").
% extract caller/callee info from  From/To value
-define(RE_EXTRACT_PHONE,"(?i)[^<]*<\\s*(?:tel|sip):\\+?(?:86)?(\\d+).*").

-record(session,{
  id,
  caller,
  callee,
  timestamp     % in milliseconds
}).

-type analyze_result() :: invalid | {ok, [{atom(),string()}]}.

init(State) ->
  Nodes = [node()],
  {ok,Dbpath} = application:get_env(sip_session_db),
  application:set_env(mnesia,dir,Dbpath),

  try
    mnesia:create_schema(Nodes),
    mnesia:start(),
    mnesia:create_table(session,[
      {type,set},
      {attributes,record_info(fields,session)},
      {index,[#session.caller,#session.callee,#session.timestamp]},
      {disc_only_copies,Nodes}
    ]),
    mnesia:wait_for_tables([session],5000),
    {ok,State}
  catch
    error:Reason ->
      logger:error("start sip session error: ~p",[Reason]),
      {error,Reason}
  end.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({packet,Packet},State) ->
  case analyze(Packet) of
    {ok,S} ->
      SR = to_session(#session{timestamp = os:system_time(millisecond)},S),
      mnesia:transaction(
	fun() ->
	    mnesia:write(SR)
	end);
    invalid -> do_nothing
  end,
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.


cast(Packet) ->
  gen_server:cast(?MODULE,{packet,Packet}).

to_session(S,[]) -> S;
to_session(S,[H|T]) ->
  SS = case H of
    {id,Id} -> S#session{id=Id};
    {caller,Caller} -> S#session{caller=Caller};
    {callee,Callee} -> S#session{callee=Callee};
    {timestamp,Timestamp} -> S#session{timestamp=Timestamp};
    _ -> S
       end,
  to_session(SS,T).

%% search the packet binary with
%% cmd is "INVITE"
%% session id
%% from with tag
%% to without tag
%% and return these info as list of tuple:
%% {ok,[{id,Id},{caller,Caller},{callee,Callee}]} in form of record #session
%% or return invalid if not a valid initial INVITE
-spec analyze(binary()) -> analyze_result().
analyze(Packet) ->
  Lines = binary:split(Packet,<<"\r\n">>,[global]),
  analyze_one(all,[],Lines).
analyze_one(_,R,_) when length(R) == 3 ->
  {ok,R};
analyze_one(_,_,[]) -> invalid;
analyze_one(_,_,[<<>> | _T]) ->  invalid;    % all headers are checked
analyze_one(all,R,[H|T]) ->
  case analyze_one(inspect_cmd,H) of
    invalid -> invalid;
    found -> analyze_one(only_header,R,T);
    not_found ->
      case analyze_one(inspect_header,H) of
	invalid -> invalid;
	not_found -> analyze_one(all,R,T);
	{found,V} -> analyze_one(all,[V | R],T)
      end
  end;
analyze_one(only_header,R,[H|T]) ->
  case analyze_one(inspect_header,H) of
    invalid -> invalid;
    not_found -> analyze_one(only_header,R,T);
    {found,V} -> analyze_one(only_header,[V | R],T)
  end.

analyze_one(inspect_cmd,Line) ->
  case re:run(Line,?RE_REQUEST_COMMAND,[{capture,[1],list}]) of
    {match,[Key]} ->
      case Key of
	"INVITE" -> found;
	_ -> invalid
      end;
    _ -> not_found
  end;
analyze_one(inspect_header,Line) ->
  case re:run(Line,?RE_REQUEST_HEADER,[{capture,[1,2],list}]) of
    {match,[Header,Value]} ->
      H = string:to_lower(Header),
      case H of
	"session-id" ->
	  {found,{id,Value}};
	"from" ->
	  case re:run(Line,?RE_HAS_TAG) of
	    {match,_} ->
	      case re:run(Line,?RE_EXTRACT_PHONE,[{capture,[1],list}]) of
		{match,[Phone]} ->
		  {found,{caller,Phone}};
		_ -> invalid
	      end;
	    _ -> invalid   % from without tag
	  end;
	"to" ->
	  case re:run(Line,?RE_HAS_TAG) of
	    {match,_} -> invalid;    % to already has tag, not first invite
	    _ ->
	      case re:run(Line,?RE_EXTRACT_PHONE,[{capture,[1],list}]) of
		{match,[Phone]} ->
		  {found,{callee,Phone}};
		_ -> invalid
	      end
	  end;
	_ -> not_found
      end;
    _ -> not_found
  end.
