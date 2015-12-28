-module(calc).
-export([parse/1]).

parse(S) ->
  parse({}, S).

parse(Acc, "") ->
  Acc;
parse(Acc, [Ch|L]) ->
  IsOp = is_op(Ch),
  IsDigit = is_digit(Ch),
  IsOpenPar = is_open_par(Ch),
  if
    IsDigit ->
      Num = {num, list_to_integer([Ch|""])},
      if
        Acc =:= {} ->
          parse(Num, L);
        true ->
          {Op, FirstPart} = Acc,
          {Op, FirstPart, Num}
      end;
    IsOp ->
      {operation(Ch), Acc, parse({}, L)};
    IsOpenPar ->
      PlusIndex = find_first(L, $+),
      MinusIndex = find_first(L, $-),
      DivIndex = find_first(L, $/),
      MultIndex = find_first(L, $*),
      if
        PlusIndex > 0 ->
          {Left, Right} = divide(L, PlusIndex),
          {operation($+), parse(Left), parse(Right)};
        MinusIndex > 0 ->
          {Left, Right} = divide(L, MinusIndex),
          {operation($-), parse(Left), parse(Right)};
        DivIndex > 0 ->
          {Left, Right} = divide(L, DivIndex),
          {operation($/), parse(Left), parse(Right)};
        MultIndex > 0 ->
          {Left, Right} = divide(L, MultIndex),
          {operation($*), parse(Left), parse(Right)};
        true ->
          parse(Acc, L -- ")")
      end
  end.

divide(S, Index) ->
  H = lists:sublist(S, Index),
  T = lists:sublist(S, Index + 2, length(S) - length(H) - 1),
  {H, T -- ")"}.

is_op(Ch) ->
  lists:member(Ch, "+-/*").

is_digit(Ch) ->
  lists:member(Ch, "0123456789").

is_open_par(Ch) ->
  Ch =:= $(.

operation(Op) ->
  case Op of
    $+ ->
      plus;
    $- ->
      minus;
    $/ ->
      divide;
    $* ->
      multiply
  end.

find_first(S, Sym) ->
  find_first(S, Sym, 0, 0).

find_first([], _, _, _) ->
  -1;
find_first([Ch|_], Sym, Index, C) when Ch =:= Sym andalso C =:= 0 ->
  Index;
find_first([Ch|L], Sym, Index, C) when Ch =:= $) andalso C > 0 ->
  find_first(L, Sym, Index + 1, C - 1);
find_first([Ch|L], Sym, Index, C) when Ch =:= $( ->
  find_first(L, Sym, Index + 1, C + 1);
find_first([_|L], Sym, Index, C) ->
  find_first(L, Sym, Index + 1, C).
