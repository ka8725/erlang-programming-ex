-module(test).
-export([sum/1, sum/2, create/1, reverse_create/1]).

sum(0) -> 0;
sum(N) -> N + sum(N - 1).

sum(N, M) when N > M -> error(badarg);
sum(X, X) -> X;
sum(N, M) -> N + sum(N + 1, M).

list_create(0) -> [];
list_create(N) -> [N|list_create(N - 1)].

create(N) -> lists:reverse(list_create(N)).
reverse_create(N) -> list_create(N).
