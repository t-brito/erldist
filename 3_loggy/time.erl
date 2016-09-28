-module(time).

-export([zero/0, inc/2, merge/2, leq/2]).
-export([clock/1, update/3, safe/2]).

zero() ->
  0.

inc(Name, T) -> T+1.

merge(Ti, Tj) when Ti >= Tj -> Ti;
merge(_, Tj) -> Tj.

leq(Ti, Tj) when Ti =< Tj -> true;
leq(_, _) -> false.

clock([]) ->
  [];
clock(Nodes) ->
  [H|T] = Nodes,
  [{H, 0}] ++ clock(T).

update(_, _, []) ->                                   % base case
  [];
update(Node, Time, [{Node, _}|RestClock]) ->          % matches
  [{Node, Time}] ++ update(Node, Time, RestClock);
update(Node, Time, Clock) ->                          % does not match
  [{N_old, T_old}|RestClock] = Clock,
  [{N_old, T_old}] ++ update(Node, Time, RestClock).

safe(_, []) ->
  true;
safe(Time, [{_, Tj}|RestClock]) when Time =< Tj ->
  safe(Time, RestClock);
safe(_, _) ->
  false.
