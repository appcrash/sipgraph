-module(sip_session).
-behaviour(gen_server).

-export([init/1,handle_call/3,handle_cast/2,start_link/0]).

-record(session,{
  id,
  caller,
  callee
}).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

init(State) ->
  Nodes = node(),
  try
    ok = mnesia:create_schema(Nodes),
    mnesia:create_table(session,[
      {attributes,record_info(fields,session)},
      {index,[#session.caller,#session.callee]},
      {disc_copies,Nodes},
      {type,set}
    ]),
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

handle_cast(_Request,State) ->
  {noreply,State}.
