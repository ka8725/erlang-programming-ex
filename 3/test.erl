-module(test).
-export([sum/1, sum/2, create/1, reverse_create/1, print1/1, print2/1,
         filter/2, reverse/1, concatenate/1, flatten/1, qsort/1, msort/1, divide2/1]).

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

divide_to_lesser_and_bigger(List, X) ->
  divide_to_lesser_and_bigger(List, X, [], []).

divide_to_lesser_and_bigger([H|L], X, Lesser, Bigger) when H < X ->
  divide_to_lesser_and_bigger(L, X, [H|Lesser], Bigger);
divide_to_lesser_and_bigger([H|L], X, Lesser, Bigger) when H >= X ->
  divide_to_lesser_and_bigger(L, X, Lesser, [H|Bigger]);
divide_to_lesser_and_bigger([], _, Lesser, Bigger) ->
  [reverse(Lesser)|reverse(Bigger)].

qsort([]) ->
  [];
qsort([H|L]) ->
  [Lesser|Bigger] = divide_to_lesser_and_bigger(L, H),
  concatenate([qsort(Lesser), H, qsort(Bigger)]).

len(List) ->
  len(List, 0).

len([], Len) ->
  Len;
len([_|L], Len) ->
  len(L, Len + 1).

divide2(List) ->
  slice(List, len(List) div 2, 0, [[]]).

slice([H|L], By, Index, [AccH|AccL]) when Index < By ->
  slice(L, By, Index + 1, [[H|AccH]|AccL]);
slice([H|L], By, Index, [AccH|AccL]) when Index >= By ->
  slice(L, By, Index + 1, [AccH|[H|AccL]]);
slice([], _, _, Acc) ->
  [H|L] = Acc,
  [reverse(H)|reverse(L)].

msort([]) ->
  [];
msort([H|[]]) ->
  [H];
msort(List) ->
  [H|L] = divide2(List),
  msort(qsort(H), qsort(L)).

msort(List1, List2) ->
  msort(List1, List2, []).

msort([H1|L1], [H2|L2], Acc) when H1 < H2 ->
  msort(L1, [H2|L2], [H1|Acc]);
msort([H1|L1], [H2|L2], Acc) when H2 =< H1 ->
  msort([H1|L1], L2, [H2|Acc]);
msort([H1|L1], [], Acc) ->
  msort(L1, [], [H1|Acc]);
msort([], [H2|L2], Acc) ->
  msort([], L2, [H2|Acc]);
msort([], [], Acc) ->
  reverse(Acc).
