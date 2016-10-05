-module(gms3).

-export([start/1, start/2]).

bcast(Id, Msg, Nodes) ->
  lists:foreach(fun(Node) -> Node ! Msg, crash(Id) end, Nodes).


crash(Id) ->
  case rand:uniform(1000) of
    1000 ->
      io:format("leader ~w: crash~n", [Id]),
      exit(no_luck);
    _ ->
      ok
  end.


leader(Id, Master, N, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      bcast(Id, {msg, N+1, Msg}, Slaves),
      Master ! Msg,
      leader(Id, Master, N+1, Slaves, Group);

    {join, Wrk, Peer} ->
      Slaves2 = lists:append(Slaves, [Peer]),
      Group2 = lists:append(Group, [Wrk]),
      bcast(Id, {view, N+1, [self()|Slaves2], Group2}, Slaves2),
      Master ! {view, Group2},
      leader(Id, Master, N+1, Slaves2, Group2);

    stop ->
      ok
  end.


slave(Id, Master, Leader, N, Last, Slaves, Group) ->
  receive
    {mcast, Msg} ->
      Leader ! {mcast, Msg},
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {join, Wrk, Peer} ->
      Leader ! {join, Wrk, Peer},
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {msg, NextN, Msg} when NextN > N ->
      Master ! Msg,
      slave(Id, Master, Leader, NextN, {msg, NextN, Msg}, Slaves, Group);

    {msg, _, _} ->  % invalid NextN
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {view, NextN, [Leader|Slaves2], Group2} when NextN > N ->
      Master ! {view, Group2},
      slave(Id, Master, Leader, NextN, {view, NextN, [Leader|Slaves2], Group2}, Slaves2, Group2);

    % account for view messages where you don't recognize leader
    {view, _, _, _} ->
      slave(Id, Master, Leader, N, Last, Slaves, Group);

    {'DOWN', _Ref, process, Leader, _Reason} ->   % message sent by erlang:monitor(process, Leader)
      election(Id, Master, N, Last, Slaves, Group);

    stop ->
      ok
  end.


start(Id) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Self) end)}.


init(Id, Master) ->
  leader(Id, Master, 0, [], [Master]).


start(Id, Grp) ->
  Self = self(),
  {ok, spawn_link(fun()-> init(Id, Grp, Self) end)}.


init(Id, Grp, Master) ->
  Self = self(),
  Grp ! {join, Master, Self},
  receive
    Msg = {view, N, [Leader|Slaves], Group} ->
      erlang:monitor(process, Leader),
      Master ! {view, Group},
      slave(Id, Master, Leader, N, Msg, Slaves, Group)
  after 5000 ->
    % wait for master to reply, otherwise quit
    Master ! {error, "no reply from leader"}
  end.

election(Id, Master, N, Last, Slaves, [_|Group]) ->
  Self = self(),
  case Slaves of
    [Self|Rest] ->
      bcast(Id, Last, Rest),
      bcast(Id, {view, N+1, Slaves, Group}, Rest),
      Master ! {view, Group},
      io:format("~w: leader stepping up. ~n", [Id]),
      leader(Id, Master, N+1, Rest, Group);
    [Leader|Rest] ->
      erlang:monitor(process, Leader),
      slave(Id, Master, Leader, N, Last, Rest, Group)
  end.
