-module(db).
-export([new/0, destroy/1, write/3, delete/2, read/2, match/2]).

new() ->
  [].

destroy(_) ->
  ok.

write(Key, Element, Db) ->
  [{Key, Element}|delete(Key, Db)].

delete(_, Db) when Db =:= [] ->
  [];
delete(Key, [{Key, _}|Tail]) ->
  delete(Key, Tail);
delete(_, Db) ->
  Db.

read(_, []) ->
  {error, instance};
read(Key, [{Key, Val}|_]) ->
  {ok, Val};
read(Key, [_|Tail]) ->
  read(Key, Tail).

match(_, []) ->
  [];
match(Val, [{Key, Val}|Tail]) ->
  [Key|match(Val, Tail)];
match(Val, [_|Tail]) ->
  match(Val, Tail).
