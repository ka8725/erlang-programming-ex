-module(calc).
-export([parse/1, print/1, eval/1]).

print({if_, IfBody}) ->
  io:format("if ~ts~n", [print(IfBody)]);
print({IfClause, {then_, {Then, {else_, Else}}}}) ->
  io_lib:format("~ts then ~ts else ~ts", [print(IfClause), print(Then), print(Else)]);
print({plus, Left, Right}) ->
  io_lib:format("(~ts + ~ts)", [print(Left), print(Right)]);
print({minus, Left, Right}) ->
  io_lib:format("(~ts - ~ts)", [print(Left), print(Right)]);
print({minus, Exp}) ->
  io_lib:format("-~ts", [print(Exp)]);
print({divide, Left, Right}) ->
  io_lib:format("(~ts / ~ts)", [print(Left), print(Right)]);
print({multiply, Left, Right}) ->
  io_lib:format("(~ts * ~ts)", [print(Left), print(Right)]);
print({num, Num}) ->
  io_lib:format("~ts", [integer_to_list(Num)]).

eval(S) ->
  evaluate(parse(S)).

evaluate({if_, IfBody}) ->
  evaluate(IfBody);
evaluate({IfClause, {then_, {Then, {else_, Else}}}}) ->
  ExecIf = evaluate(IfClause),
  if
    ExecIf =:= 0 ->
      evaluate(Then);
    true ->
      evaluate(Else)
  end;
evaluate({plus, Left, Right}) ->
  evaluate(Left) + evaluate(Right);
evaluate({minus, Left, Right}) ->
  evaluate(Left) - evaluate(Right);
evaluate({minus, Exp}) ->
  -evaluate(Exp);
evaluate({divide, Left, Right}) ->
  evaluate(Left) / evaluate(Right);
evaluate({multiply, Left, Right}) ->
  evaluate(Left) * evaluate(Right);
evaluate({num, Num}) ->
  Num.

parse(S) ->
  parse_tokens(string:tokens(S, " ")).

parse_tokens(["if"|L]) ->
  {if_, parse_tokens(L)};
parse_tokens(["then"|L]) ->
  {then_, parse_tokens(L)};
parse_tokens(["else"|L]) ->
  {else_, parse_tokens(L)};
parse_tokens([H|[]]) ->
  parse({}, H);
parse_tokens([H|L]) ->
  {parse({}, H), parse_tokens(L)}.

parse(Acc, "") ->
  Acc;
parse(Acc, "~n") ->
  Acc;
parse(Acc, [Ch|L]) ->
  IsOp = is_op(Ch),
  IsDigit = is_digit(Ch),
  IsOpenPar = Ch =:= $(,
  IsUnaryMinus = Ch =:= $~,
  if
    IsUnaryMinus ->
      {operation($-), parse({}, L)};
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
          {operation($+), parse({}, Left), parse({}, Right)};
        MinusIndex > 0 ->
          {Left, Right} = divide(L, MinusIndex),
          {operation($-), parse({}, Left), parse({}, Right)};
        DivIndex > 0 ->
          {Left, Right} = divide(L, DivIndex),
          {operation($/), parse({}, Left), parse({}, Right)};
        MultIndex > 0 ->
          {Left, Right} = divide(L, MultIndex),
          {operation($*), parse({}, Left), parse({}, Right)};
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
