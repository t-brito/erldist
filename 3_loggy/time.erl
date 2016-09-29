-module(time).

-export([zero/0, inc/2, merge/2, leq/2]).
-export([clock/1, update/3, safe/2]).

%zero() -> 0.
zero() -> [{john, 0}, {paul, 0}, {ringo, 0}, {george, 0}].

inc(_, []) -> [];
inc(Node, [{Node, Ti}|Tail]) -> [{Node, Ti+1}] ++ Tail;
inc(Node, [H|T]) -> [H] ++ inc(Node, T).

merge([], []) -> [];
merge([{Node, Ti}|Taili], [{Node, Tj}|Tailj]) ->
  case Ti =< Tj of
    true -> [{Node, Tj}] ++ merge(Taili, Tailj);
    false -> [{Node, Ti}] ++ merge(Taili, Tailj)
  end.

leq([], []) -> true;
leq([{_, Ti}|Taili], [{_, Tj}|Tailj]) when Ti =< Tj -> leq(Taili, Tailj);
leq(_, _) -> false.

clock([]) ->
  [];
clock(Nodes) ->
  [H|T] = Nodes,
  [{H, 0}] ++ clock(T).

update(_, _, []) ->                                         % base case
  [];
update(Node, [{Node, Time}|_], [{Node, _}|RestClock]) ->    % matches name both on clock and on time
  [{Node, Time}] ++ update(Node, Time, RestClock);
update(Node, Time, [{Node, Tc}|RestClock]) ->               % matches name on clock but not on time
  [_|RestTime] = Time,
  update(Node, RestTime, [{Node, Tc}|RestClock]);
update(Node, Time, Clock) ->                                % does not match
  [{N_old, T_old}|RestClock] = Clock,
  [{N_old, T_old}] ++ update(Node, Time, RestClock).

%safe(_, []) -> true;
%safe(Time, [{_, Tj}|RestClock]) when Time =< Tj -> safe(Time, RestClock);
%safe(_, _) -> false.

safe(Time, Clock) -> leq(Time, Clock).
