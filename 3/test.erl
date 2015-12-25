-module(test).
-export([sum/1, sum/2, create/1, reverse_create/1, print1/1, print2/1,
         filter/2, reverse/1, concatenate/1, flatten/1]).

sum(0) -> 0;
sum(N) -> N + sum(N - 1).

sum(N, M) when N > M -> error(badarg);
sum(X, X) -> X;
sum(N, M) -> N + sum(N + 1, M).

list_create(0) -> [];
list_create(N) -> [N|list_create(N - 1)].

create(N) -> lists:reverse(list_create(N)).
reverse_create(N) -> list_create(N).


print1(0) -> do_nothing;
print1(N) when N < 0 -> error(badarg);
print1(N) ->
  print1(N - 1),
  io:format("Number: ~p~n", [N]).


print2(0) -> print1(0);
print2(N) when N < 0 -> print1(N);
print2(N) when N rem 2 =:= 0 -> print2(N - 1);
print2(N) when N rem 2 =/= 0 ->
  print2(N - 1),
  io:format("Number: ~p~n", [N]).


filter([], _) -> [];
filter([Head|Tail], N) when Head =< N -> [Head|filter(Tail, N)];
filter([Head|Tail], N) when Head > N -> filter(Tail, N).


reverse(List) ->
  reverse(List, []).

reverse([], Acc) ->
  Acc;
reverse([Head|Tail], Acc) ->
  reverse(Tail, [Head|Acc]).


concatenate(Lists) ->
  concatenate(Lists, []).

concatenate([], Acc) ->
  Acc;
concatenate([[]|Tail], Acc) -> concatenate(Tail, Acc);
concatenate([[H|T]|Tail], Acc) ->
  [H|concatenate(T, concatenate(Tail, Acc))];
concatenate([H|T], Acc) ->
  [H|concatenate(T, Acc)].


flatten(Lists) ->
  flatten(Lists, []).

flatten([], Acc) ->
  Acc;
flatten([Head|Tail], Acc) ->
  concatenate([flatten(Head, Acc), flatten(Tail, Acc)]);
flatten(X, Acc) ->
  [X|Acc].
