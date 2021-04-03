-module(packet).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([handle/1]).



init(State) ->
  {ok,State}.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({packet,Packet},State) ->
  sip_db:store(Packet),
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.

handle(Packet) ->
  gen_server:cast(?MODULE,{packet,Packet}).
