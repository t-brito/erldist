-module(node2).

-export([start/1, start/2]).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create()).

connect(Id, nil) ->
  {ok, {Id, self()}};

connect(_, Peer) ->
  Qref = make_ref(),
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}
  after 5000 ->
    io:format("Time out: no response~n",[])
  end.



schedule_stabilize() ->
  timer:send_interval(1000, self(), stabilize).


node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);

    {notify, New} ->                            % msg from Potential new predecessor
      {Pred, NewStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, NewStore);

    {request, Peer} ->                          % msg from old predecessor (stabilize)
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);

    {status, Pred} ->                           % message from old Successor (stabilize)
      Succ = stabilize(Pred, Id, Successor),    % stabilize link
      node(Id, Predecessor, Succ, Store);

    stabilize ->                                % periodically called
      stabilize(Successor),
      node(Id, Predecessor, Successor, Store);

    print ->
      io:format("me: ~w, pred: ~w, succ: ~w~n", [Id, Predecessor, Successor]),
      node(Id, Predecessor, Successor, Store);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor, Store);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor, Store);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor, Store);

    {add, Key, Value, Qref, Client} ->
      Added = add(Key, Value, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Added);

    {lookup, Key, Qref, Client} ->
      lookup(Key, Qref, Client, Id, Predecessor, Successor, Store),
      node(Id, Predecessor, Successor, Store);

    {handover, Elements} ->
      Merged = storage:merge(Store, Elements),
      node(Id, Predecessor, Successor, Merged)

  end.



add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.



create_probe(Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

remove_probe(T, Nodes) ->
  T2 = erlang:system_time(micro_seconds),
  io:format("message around ~w took ~w microseconds.~n", [Nodes, T2-T]).

forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Ref, lists:append(Nodes,[Id]), T}.


stabilize({_, Spid}) ->
  Spid ! {request, self()}.

request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};

    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.


notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};

    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};
        false ->
          {Predecessor, Store}
      end
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

stabilize(Pred, Id, Successor) ->               % stabilize link - return new successor
  {Skey, Spid} = Successor,
  case Pred of
    nil ->                                      % if Successor has no Predecessor
      Spid ! {notify, {Id, self()}},
      Successor;

    {Id, _} ->                                  % if Successor's predecessor is us
      Successor;

    {Skey, _} ->                                % if Successor's predecessor is itself
      Spid ! {notify, {Id, self()}},
      Successor;

    {Xkey, Xpid} ->                     % if Successor's predecessor is someone else
      case key:between(Xkey, Id, Skey) of
        true ->
          stabilize({Xkey, Xpid}),
          {Xkey, Xpid};
        false ->
          Spid ! {notify, {Id, self()}},
          Successor
      end
  end.
