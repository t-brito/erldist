-module(time).

-export([zero/0, inc/2, merge/2, leq/2]).

zero() ->
  0.

inc(Name, T) -> T+1.

merge(Ti, Tj) when Ti >= Tj -> Ti;
merge(_, Tj) -> Tj.

leq(Ti, Tj) when Ti =< Tj -> true;
leq(_, _) -> false.
