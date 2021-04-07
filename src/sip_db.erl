-module(sip_db).

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0,terminate/2]).
-export([store/1,read_prefix/2]).
-define(LEVELDB_OPEN_OPTION,[{create_if_missing,true}]).
-define(MAX_ITEM_READ_SIZE,200).

-define(RE_SESSION_ID,"(?i)^session-id:\\s*([^\\s]+)\\s*$").

-record(state,{
  db
}).

-type analyze_result() :: invalid | {ok,binary(),binary()}.


init(_) ->
  ets:new(session_info,[named_table,set]),
  {ok,Path} = application:get_env(leveldb),
  case eleveldb:open(Path,?LEVELDB_OPEN_OPTION) of
    {ok,Db} ->
      State = #state{db = Db},
      {ok,State};
    {error,Reason} -> {stop,Reason}
  end.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call({read_prefix,{Key,Size}},_From,#state{db = Db} = State) ->
  R = if
	is_integer(Size) andalso Size > 0 ->
	  S = erlang:min(Size,?MAX_ITEM_READ_SIZE),
	  iterate_kv(Db,Key,S);
	true -> []
      end,
  {reply,R,State};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({store,Packet},#state{db = Db}=State) ->
  case analyze(Packet) of
    {ok,Key,OriginPacket} ->
      case eleveldb:put(Db,Key,OriginPacket,[{sync,false}]) of    % store whole packet
	{error,Reason} ->
	  logger:error("leveldb write error: ~p",[Reason]);
	ok -> ok
      end;
    invalid -> do_nothing
  end,
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.

terminate(_Reason,#state{db = Db} = _State) ->
  case eleveldb:close(Db) of
    {error,Reason} ->
      logger:error("level db close with error ~p",[Reason]);
    R -> R
  end.

-spec store(binary()) -> ok.
store(Packet) ->
  gen_server:cast(?MODULE,{store,Packet}).


-spec read_prefix(binary(),integer()) -> list().
read_prefix(Key,Size) ->
  gen_server:call(?MODULE,{read_prefix,{Key,Size}}).

iterate_kv(Db,Key,Size) ->
  {ok,I} = eleveldb:iterator(Db,[]),
  iterate_one(I,Key,[],Size).

%% read at most *Size* of item that start with *Key* from leveldb
iterate_one(I,Prefix,L,Size) when is_list(L) andalso Size > 0 ->
  Action = case L of
    [] -> Prefix;
    _ -> next
  end,
  case eleveldb:iterator_move(I,Action) of
    {ok,K,V} ->
      Len = byte_size(Prefix),
      case binary:longest_common_prefix([Prefix,K]) of
	Len -> iterate_one(I,Prefix,[{K,V} | L],Size - 1);   % prefix matched
	_ -> iterate_one(I,Prefix,L,0)
      end;
    {error,_} -> iterate_one(I,Prefix,L,0)    % end iterating, close iterator
  end;
iterate_one(I,_Prefix,L,_Size) ->
  %logger:info("iterate_one ended here: ~p ~p",[L,_Size]),
  eleveldb:iterator_close(I),
  lists:reverse(L).

%% first line of packet would always be in form of:
%% session-id: xxxxxx
%% otherwise it is not recognized by us
-spec analyze(binary()) -> analyze_result().
analyze(Packet) ->
  case binary:split(Packet,<<"\r\n">>) of  % split only cut in first occurence once
    [SL,OriginPacket] ->
      case re:run(SL,?RE_SESSION_ID,[{capture,[1],list}]) of
	{match,[Sid]} -> % session id found
	  case ets:lookup(session_info,Sid) of
	    [{_,Seq}] ->
	      ets:update_counter(session_info,Sid,{2,1}),  % increase seq counter by 1
	      Key = list_to_binary(io_lib:format("~s_~4..0B",[Sid,Seq])), % seq number with 4 characters wide padding 0
	      {ok,Key,OriginPacket};
	    _ ->  % the first session seen, track it as new session, counter starts at 1
	      ets:insert(session_info,{Sid,1}),
	      Key = list_to_binary(io_lib:format("~s_~4..0B",[Sid,1])), % seq number with 4 characters wide padding 0
	      {ok,Key,OriginPacket}
	  end;
	_ ->  % can not recognized first line as session id
	  logger:error("invalid packet without session id, first line: ~p",[SL]),
	  invalid
      end;
    _ -> % invalid packet, can not even be split
      logger:error("invalid packet: ~p",[Packet]),
      invalid
  end.
