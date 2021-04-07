-module(web).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).



init(State) ->
  Dispatch = cowboy_router:compile([
    {'_',[
      {"/sip/:operation",web_handler,[]}
    ]}
  ]),
  {_,Port} = application:get_env(http_port),
  case cowboy:start_clear(http,[{port,Port}],#{
  env => #{dispatch => Dispatch}}) of
    {ok,_} ->
      {ok,State};
    {error,Reason} -> {error,Reason}
  end.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast(_Request,State) ->
  {noreply,State}.