-module(sip_db).

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([store/4,read_prefix/3]).

-include("common.hrl").
-include_lib("stdlib/include/ms_transform.hrl").

-define(MAX_ITEM_READ_SIZE,200).


init(State) ->
  {ok,State}.

start_link() ->
  gen_server:start_link({local,?MODULE},?MODULE,[],[]).

handle_call({read_prefix,{Key,Size}},_From,State) ->
  R = if
	is_integer(Size) andalso Size > 0 ->
	  %S = erlang:min(Size,?MAX_ITEM_READ_SIZE),
	  MatchSpec = ets:fun2ms(
			fun(#signal{signal_id=#signal_key{session=Sid}} = Record) when Sid == Key -> Record end
		       ),
	  case mnesia:transaction(
	    fun() ->
		mnesia:select(signal,MatchSpec)
	    end) of
	    {atomic,Res} -> Res;
	    {aborted,Reason} ->
	      logger:error("read_prefix failed with reason: ~p",[Reason]),
	      []
	    end;
	true -> []
      end,
  {reply,R,State};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

%% handle_cast({store,SessionId,Seq,OriginPacket},State) ->
%%   Ts = os:system_time(millisecond),
%%   Record = #signal{signal_id = #signal_key{session = SessionId,seq = Seq},
%% 		   timestamp = Ts,data = OriginPacket},
%%   mnesia:dirty_write(Record),
%%   {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.


%% ################   API   ##################
-spec store(string(),integer(),binary(),integer()) -> ok.
store(SessionId,Seq,Packet,Ts) ->
  Record = #signal{signal_id = #signal_key{session = SessionId,seq = Seq},
		   timestamp = Ts,data = Packet},
  mnesia:write(Record).

-spec read_prefix(string(),integer(),integer()) -> [#signal{}].
read_prefix(Key,Size,Timeout) ->
  gen_server:call(?MODULE,{read_prefix,{Key,Size}},Timeout).

%% ################   API   ##################
