-module(my_db).
-export([start/0, write/2, read/1, delete/1, stop/0, match/1]).
-export([init/1, terminate/1]).

start() ->
  register(my_db, spawn(my_db, init, [db:new()])),
  ok.

init(Db) ->
  receive
    {write, Pid, {K, V}} ->
      Pid ! ok,
      init(db:write(K, V, Db));
    {read, Pid, K} ->
      Pid ! db:read(K, Db),
      init(Db);
    {match, Pid, V} ->
      Pid ! db:match(V, Db),
      init(Db);
    {delete, Pid, K} ->
      Pid ! ok,
      init(db:delete(K, Db));
    {stop, Pid} ->
      Pid ! ok,
      terminate(Db)
  end.

write(K, V) ->
  my_db ! {write, self(), {K, V}},
  receive ok -> ok end.

read(K) ->
  my_db ! {read, self(), K},
  receive Val -> Val end.

delete(K) ->
  my_db ! {delete, self(), K},
  receive ok -> ok end.

stop() ->
  my_db ! {stop, self()},
  receive ok -> ok end.

match(V) ->
  my_db ! {match, self(), V},
  receive Val -> Val end.

terminate(Db) ->
  db:destroy(Db).
