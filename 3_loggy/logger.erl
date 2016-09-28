-module(logger).

-export([start/1, stop/1]).

start(Nodes) ->
  spawn_link(fun() -> init(Nodes) end).

stop(Logger) ->
  Logger ! stop.

init(Nodes) ->
  loop(time:clock(Nodes), []).

loop(Clock, Queue) ->
  receive
    {log, From, Time, Msg} ->
      % should never worry about receiving lower Time for same Node
      NewClock = time:update(From, Time, Clock),
      NewQueue = logAndRemove(NewClock, Queue ++ [{From, Time, Msg}]),
      loop(NewClock, NewQueue);
    stop ->
      ok
  end.

logAndRemove(_, []) ->
  [];
logAndRemove(Clock, [{From, Time, Msg}|RestQueue]) ->
  case time:safe(Time, Clock) of
    true ->
      log(From, Time, Msg),
      logAndRemove(Clock, RestQueue);
    false ->
      [{From, Time, Msg}] ++ logAndRemove(Clock, RestQueue)
  end.

log(From, Time, Msg) ->
  io:format("log: ~w ~w ~p~n", [Time, From, Msg]).
