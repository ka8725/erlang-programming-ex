-module(test).
-export([sum/1, sum/2, create/1, reverse_create/1, print1/1, print2/1]).

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
