%%%-------------------------------------------------------------------
%% @doc sipgraph public API
%% @end
%%%-------------------------------------------------------------------

-module(sipgraph_app).

-behaviour(application).

-export([start/2, stop/1]).

-include("common.hrl").

start(_StartType, _StartArgs) ->
  case init_mnesia() of
    {error,R} -> {error,R};
    ok ->
      web:start_link(),
      sipgraph_sup:start_link()
  end.

stop(_State) ->
    ok.

init_mnesia() ->
  Nodes = [node()],
  {ok,Dbpath} = application:get_env(sip_session_db),
  application:set_env(mnesia,dir,Dbpath),
  mnesia:create_schema(Nodes),
  mnesia:start(),
  mnesia_rocksdb:register(),

  try
    mnesia:create_table(session,[
      {type,set},
      {attributes,record_info(fields,session)},
      {index,[#session.caller,#session.callee,#session.timestamp]},
      {rocksdb_copies,Nodes}
    ]),
    mnesia:create_table(signal,[
      {type,set},
      {attributes,record_info(fields,signal)},
      {index,[#signal.timestamp]},
      {rocksdb_copies,Nodes}
    ]),

    mnesia:wait_for_tables([session,signal],10000),
    ok
  catch
    error:Reason ->
      logger:error("start sip session error: ~p",[Reason]),
      {error,Reason}
  end.




%% internal functions
