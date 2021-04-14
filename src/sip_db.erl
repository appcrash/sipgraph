-module(sip_db).

-behaviour(gen_server).
-export([init/1,handle_call/3,handle_cast/2,start_link/0]).
-export([store/3,read_prefix/2]).

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
	  mnesia:dirty_select(signal,MatchSpec);
	true -> []
      end,
  {reply,R,State};
handle_call(Request,_From,State) ->
  {reply,Request,State}.

handle_cast({store,SessionId,Seq,OriginPacket},State) ->
  Ts = os:system_time(millisecond),
  Record = #signal{signal_id = #signal_key{session = SessionId,seq = Seq},
		   timestamp = Ts,data = OriginPacket},
  mnesia:dirty_write(Record),
  {noreply,State};
handle_cast(_Request,State) ->
  {noreply,State}.


%% ################   API   ##################
-spec store(string(),integer(),binary()) -> ok.
store(Session,Seq,Packet) ->
  gen_server:cast(?MODULE,{store,Session,Seq,Packet}).

-spec read_prefix(string(),integer()) -> [#signal{}].
read_prefix(Key,Size) ->
  gen_server:call(?MODULE,{read_prefix,{Key,Size}}).

%% ################   API   ##################
