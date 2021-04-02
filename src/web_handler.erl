-module(web_handler).
-export([init/2]).

init(Req,Opts) ->
    case cowboy_req:binding(operation,Req) of
      <<"get">> ->
	sip_get(Req,Opts);
      undefined ->
	sip_error(400,Req)
    end.


sip_error(ErrNum,Req) ->
  cowboy_req:reply(ErrNum,Req).

sip_get(Req,Opts) ->
  try
    Qs = cowboy_req:match_qs([{key,nonempty},{size,int,0}],Req),
    logger:info("qs is ~p ",[Qs]),
    Req = cowboy_req:reply(200,
			   #{
			     <<"conntent-type">> => <<"text/plain,charset=utf-8">>
			    },<<"1">>,Req),
    {ok,Req,Opts}
  catch
    error:{badkey,K} ->
      logger:error("bad request with key: ~p",[K]),
      Req1 = sip_error(400,Req),
      {ok,Req1,Opts}
  end.
