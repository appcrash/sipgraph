-module(sip_db).

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0,terminate/2]).
-export([store/2,read_prefix/2]).
-define(LEVELDB_OPEN_OPTION,[{create_if_missing,true}]).
-define(MAX_ITEM_READ_SIZE,200).
-record(state,{
  db
}).


init(_) ->
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

handle_cast({store,Key,Value},#state{db = Db}=State) ->
  case eleveldb:put(Db,Key,Value,[{sync,false}]) of
    {error,Reason} ->
      logger:error("leveldb write error: ~p",[Reason]);
    ok -> ok
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

-spec store(binary(),binary()) -> ok.
store(Key,Value) ->
  gen_server:cast(?MODULE,{store,Key,Value}).

-spec read_prefix(binary(),integer()) -> list().
read_prefix(Key,Size) ->
  gen_server:call(?MODULE,{read_prefix,{Key,Size}}).

iterate_kv(Db,Key,Size) ->
  case eleveldb:iterator(Db,[]) of
    {ok,I} -> % iterator created successfully
      iterate_one(I,Key,[],Size);
    _ -> []
  end.

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
  if
    is_binary(L) -> [];   % first call with key is binary, size is 0, nothing retrieved
    true -> lists:reverse(L)
  end.
