-module(db2).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
  [].

destroy(_) ->
  ok.

write(Key, Element, Db) ->
  [{Key, Element}|delete(Key, Db)].

delete(Key, Db) ->
  lists:keydelete(Key, 1, Db).

read(Key, Db) ->
  read(Key, lists:keyfind(Key, 1, Db)).

read(Key, false) ->
  {error, instance}.
read(Key, {Key, Val}) ->
  {ok, Val}.

match(Val, Db) ->
  lists:keyfind(Val, 2, Db).
