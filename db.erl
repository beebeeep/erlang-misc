-module(db).
-export([start/2, stop/1, write/3, delete/2, read/2, match/2, dump/1]).
-export([init/1]).


%%% module external interface %%%

init(DB) -> 
    loop(DB).

start(Name, DB) -> 
    Pid = spawn(db, init, [DB]),
    register(Name, Pid),
    ok.

stop(Name) -> 
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

read(Name, Key) ->
  Name ! {read, self(), Key},
  receive {reply, Reply} -> Reply end.

write(Name, Key, Value) ->
  Name ! {write, self(), Key, Value},
  receive {reply, Reply} -> Reply end.

delete(Name, Key) ->
  Name ! {delete, self(), Key},
  receive {reply, Reply} -> Reply end.

match(Name, Value) ->
  Name ! {match, self(), Value},
  receive {reply, Reply} -> Reply end.

dump(Name) ->
  Name ! {dump, self()},
  receive {reply, Reply} -> Reply end.


%%% internal functions %%%

reply(To, Reply) -> To ! {reply, Reply}.

loop(DB) -> 
    receive 
        {read, From, Key} -> 
          Reply = db_read(Key, DB),
          reply(From, Reply),
          loop(DB);
        {write, From, Key, Value} -> 
          NewDB = db_write(Key, Value, DB),
          reply(From, ok),
          loop(NewDB);
        {delete, From, Key} ->
          NewDB = db_delete(Key, DB),
          reply(From, ok),
          loop(NewDB);
        {match, From, Value} ->
          Reply = db_match(Value, DB),
          reply(From, Reply),
          loop(DB);
        {dump, From} ->
          reply(From, DB),
          loop(DB);
        {stop, From} -> 
          reply(From, ok)
    end.


db_write(Key, Value, Db) -> 
  [{Key, Value}|Db].

db_delete(_Key, []) -> 
  [];
db_delete(KeyToDelete, Db) -> 
  [Record|RestDb] = Db,
  case Record of
    {KeyToDelete, _Value} -> RestDb;
    {Key, Value} -> [{Key, Value}|db_delete(KeyToDelete, RestDb)]
  end.

db_read(_Key, []) ->
  {error, "No value match specified key"};
db_read(Key, Db) -> 
  [Record|RestDb] = Db,
  case Record of
    {Key, Value} -> Value;
    {_K, _V} -> db_read(Key, RestDb)
  end.

db_match(_Value, []) -> 
  [];
db_match(Value, Db) -> 
  [Record|RestDb] = Db,
  case Record of 
    {Key, Value} -> [Key|db_match(Value, RestDb)];
    {_K, _V} -> db_match(Value,RestDb)
  end.

