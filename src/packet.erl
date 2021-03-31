-module(packet).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([handle/1]).

-record(state,{
  db
}).

-define(LEVELDB_OPEN_OPTION,[{create_if_missing,true}]).

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

handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({packet,Packet},State=#state{db = Db}) ->
  store(Db,Packet),
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.

handle(Packet) ->
  gen_server:cast(?MODULE,{packet,Packet}).

store(Db,Packet) ->
  logger:info("store with value ~p",[Packet]),
  case eleveldb:write(Db,[{put,<<"ppp">>,Packet}],[{sync,false}]) of
    {error,Reason} ->
      logger:error("leveldb write error: ~p",[Reason]);
    ok -> ok
  end.
