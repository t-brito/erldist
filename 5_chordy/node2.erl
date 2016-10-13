-module(node2).

-export([start/1, start/2]).

start(Id) ->
  start(Id, nil).

start(Id, Peer) ->
  timer:start(),
  spawn(fun() -> init(Id, Peer) end).

% set Successor when node is added, predecessor will be added via Stabilize
init(Id, Peer) ->
  Predecessor = nil,
  {ok, Successor} = connect(Id, Peer),
  schedule_stabilize(),
  node(Id, Predecessor, Successor, storage:create()).

% we are the first node, our successor is ourselves
connect(Id, nil) ->
  {ok, {Id, self()}};

% we are joining a ring, send Peer our own PID, have them return their Key
connect(_, Peer) ->
  Qref = make_ref(),              % Qref allows multiple key messages to be sent and received without interfering with each other
  Peer ! {key, Qref, self()},
  receive
    {Qref, Skey} ->
      {ok, {Skey, Peer}}          % now we have Skey and Spid
  after 10000 ->
    io:format("Time out: no response~n",[])
  end.



schedule_stabilize() ->
  timer:send_interval(1000, self(), stabilize).


node(Id, Predecessor, Successor, Store) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor, Store);

    % msg from new predecessor
    {notify, New} ->
      {Pred, NewStore} = notify(New, Id, Predecessor, Store),
      node(Id, Pred, Successor, NewStore);

    % msg from old predecessor requesting status (stabilize)
    {request, Peer} ->
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor, Store);

    % message from old successor returning its current predecessor (stabilize)
    {status, Pred} ->
      Succ = stabilize(Pred, Id, Successor),
      node(Id, Predecessor, Succ, Store);

    stabilize ->
      stabilize(Successor),
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


% add value if node is responsible for this key (i.e. if it's between its predecessor and itself) - otherwise pass it along, with the Qref and the Client pointers
add(Key, Value, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Client ! {Qref, ok},
      storage:add(Key, Value, Store);
    false ->
      Spid ! {add, Key, Value, Qref, Client},
      Store
  end.

% retrieve value is node is responsible for this key, otherwise pass request along, with Qref and requesting Client pointers
lookup(Key, Qref, Client, Id, {Pkey, _}, {_, Spid}, Store) ->
  case key:between(Key, Pkey, Id) of
    true ->
      Result = storage:lookup(Key, Store),
      Client ! {Qref, Result};
    false ->
      Spid ! {lookup, Key, Qref, Client}
  end.



% create probe message, add first node ID and record time
create_probe(Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Id, [Id], erlang:system_time(micro_seconds)}.

% when full circle, remove from queue and display time and full Node list
remove_probe(T, Nodes) ->
  T2 = erlang:system_time(micro_seconds),
  io:format("message around ~w took ~w microseconds.~n", [Nodes, T2-T]).

% all other nodes simply append themselves to list and forward probe along the ring
forward_probe(Ref, T, Nodes, Id, Successor) ->
  {_, Spid} = Successor,
  Spid ! {probe, Ref, lists:append(Nodes,[Id]), T}.


% send request_status message to [old] Successor
stabilize({_, Spid}) ->
  Spid ! {request, self()}.


% send status (who is current Predecessor) message to [old] Predecessor who requested it
request(Peer, Predecessor) ->
  case Predecessor of
    nil ->
      Peer ! {status, nil};

    {Pkey, Ppid} ->
      Peer ! {status, {Pkey, Ppid}}
  end.


% process notify message from Predecessor candidate - decide whether to update
notify({Nkey, Npid}, Id, Predecessor, Store) ->
  case Predecessor of
    nil ->
      Keep = handover(Id, Store, Nkey, Npid),
      {{Nkey, Npid}, Keep};

    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of     % is NewPred more recent than CurrPred
        true ->
          Keep = handover(Id, Store, Nkey, Npid),
          {{Nkey, Npid}, Keep};                       % return NewPred
        false ->
          {Predecessor, Store}                         % return CurrPred
      end
  end.

handover(Id, Store, Nkey, Npid) ->
  {Keep, Rest} = storage:split(Id, Nkey, Store),
  Npid ! {handover, Rest},
  Keep.

% Stabilize node, return new successor
stabilize(Pred, Id, Successor) ->
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

    {Xkey, Xpid} ->                         % if Successor's predecessor is someone else
      case key:between(Xkey, Id, Skey) of
        true ->                           % ahead of us
          stabilize({Xkey, Xpid}),      % recursively check until new successor found
          {Xkey, Xpid};
        false ->
          Spid ! {notify, {Id, self()}},  % behind us - we are the new predecessor
          Successor
      end
  end.
