-module(web_handler).
-export([init/2]).

init(Req,Opts) ->
    case cowboy_req:binding(operation,Req) of
      <<"get">> ->
	sip_get(Req,Opts);
      <<"put">> ->
	sip_put(Req,Opts);
      undefined ->
	sip_error(400,Req)
    end.


sip_error(ErrNum,Req) ->
  cowboy_req:reply(ErrNum,Req).

sip_get(Req,Opts) ->
  try
    #{key := Key,size := Size} = cowboy_req:match_qs([{key,nonempty},{size,int,0}],Req),
    logger:info("key is ~p, size is ~p",[Key,Size]),
    Kvlist = sip_db:read_prefix(Key,Size),
    J = jsone:encode(Kvlist),
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

sip_put(Req,Opts) ->
  try
    #{key := K,value := V} = cowboy_req:match_qs([{key,nonempty},{value,nonempty}],Req),
    logger:info("storing key:~p value:~p",[K,V]),
    sip_db:store(K,V),
    Req1 = cowboy_req:reply(200,
			    #{
			      <<"content-type">> => <<"text/plain,charset=utf-8">>
			     },<<"ok">>,Req),
    {ok,Req1,Opts}
  catch
    error:_ ->
      Req2 = sip_error(400,Req),
      {ok,Req2,Opts}
  end.
