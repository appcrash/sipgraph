-module(metric).
-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([count/1,info/0]).

init(State) ->
  %% record is counter:
  %% {Counter_Name :: atom(), Counter_Value :: integer()}
  ets:new(metric_info,[named_table,set]),
  {ok,State}.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call(info,_From,State) ->
  R = ets:foldl(
	fun({K,V},Map) -> maps:put(K,V,Map) end,
	#{},metric_info),
  {reply,R,State};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({counter,Key},State)->
  case ets:member(metric_info,Key) of
    true ->
      ets:update_counter(metric_info,Key,{2,1});
    false ->
      ets:insert(metric_info,{Key,1})
  end,
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.


-spec count(atom()) -> ok.
count(Key) ->
  gen_server:cast(?MODULE,{counter,Key}).

-spec info() -> #{atom() => integer()}.
info() ->
  gen_server:call(?MODULE,info).
