-module(worker).

-export([start/5, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter) ->
  spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter) end).

stop(Worker) ->
  Worker ! stop.

init(Name, Log, Seed, Sleep, Jitter) ->
  rand:seed(exs64, {Seed, Seed, Seed}),
  OwnTime = time:zero(),
  receive
    {peers, Peers} ->
      loop(Name, Log, Peers, Sleep, Jitter, OwnTime);
    stop ->
      ok
  end.

peers(Wrk, Peers) ->
  Wrk ! {peers, Peers}.

loop(Name, Log, Peers, Sleep, Jitter, OwnTime) ->
  Wait = rand:uniform(Sleep),
  receive
    {msg, RecvTime, Msg} ->
      NewTime = time:inc(Name, time:merge(RecvTime, OwnTime)),
      Log ! {log, Name, NewTime, {received, Msg}},
      loop(Name, Log, Peers, Sleep, Jitter, NewTime);
    stop ->
      ok;
    Error ->
      Log ! {log, Name, time, {error, Error}}
  after Wait ->
      Selected = select(Peers),
      Time = time:inc(Name, OwnTime),
      Message = {hello, rand:uniform(1000)},
      Selected ! {msg, Time, Message},
      jitter(Jitter),
      Log ! {log, Name, Time, {sending, Message}},
      loop(Name, Log, Peers, Sleep, Jitter, Time)
  end.

select(Peers) ->
  lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).
