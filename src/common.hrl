-record(session,
{
 id :: string() | atom(),
 caller :: string() | atom(),
 callee :: string() | atom(),
 timestamp :: integer() | atom()   % in milliseconds
}).

-record(signal_key,
{
 session :: string(),
 seq :: integer()
}).

-record(signal,
{
 signal_id :: #signal_key{},
 timestamp :: integer(),   % in milliseconds
 data :: binary()	   % original signalling
}).


-define(SESSION_EXPIRE_TIME,600000).		% session expiration check in 10-mins
