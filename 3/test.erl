-module(test).
-export([sum/1, sum/2]).

sum(0) -> 0;
sum(N) -> N + sum(N - 1).

sum(N, M) when N > M -> error(badarg);
sum(X, X) -> X;
sum(N, M) -> N + sum(N + 1, M).
