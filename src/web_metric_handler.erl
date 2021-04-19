-module(web_metric_handler).
-export([init/2]).

%% expose all metric data
init(Req,Opts) ->
  metric_data(Req,Opts).

metric_data(Req,Opts) ->
  Info = metric:info(),
  logger:info("info is ~p",[Info]),
  J = jsone:encode(Info),
  Req1 = cowboy_req:reply(200,
			  #{
			    <<"content-type">> => <<"application/json,charset=utf-8">>
			   },J,Req),
  {ok,Req1,Opts}.
