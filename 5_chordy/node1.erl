-module(node1).

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
  node(Id, Predecessor, Successor).

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
%  timer:send_interval(3000, self(), print),
  timer:send_interval(1000, self(), stabilize).


node(Id, Predecessor, Successor) ->
  receive
    {key, Qref, Peer} ->
      Peer ! {Qref, Id},
      node(Id, Predecessor, Successor);

    {notify, New} ->                            % msg from Potential new predecessor
      Pred = notify(New, Id, Predecessor),
      node(Id, Pred, Successor);

    {request, Peer} ->                          % msg from old predecessor (stabilize)
      request(Peer, Predecessor),
      node(Id, Predecessor, Successor);

    {status, Pred} ->                           % message from old Successor (stabilize)
      Succ = stabilize(Pred, Id, Successor),    % stabilize link
      node(Id, Predecessor, Succ);

    stabilize ->                                % periodically called
      stabilize(Successor),
      node(Id, Predecessor, Successor);

    print ->
      io:format("me: ~w, pred: ~w, succ: ~w~n", [Id, Predecessor, Successor]),
      node(Id, Predecessor, Successor);

    probe ->
      create_probe(Id, Successor),
      node(Id, Predecessor, Successor);

    {probe, Id, Nodes, T} ->
      remove_probe(T, Nodes),
      node(Id, Predecessor, Successor);

    {probe, Ref, Nodes, T} ->
      forward_probe(Ref, T, Nodes, Id, Successor),
      node(Id, Predecessor, Successor)

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


notify({Nkey, Npid}, Id, Predecessor) ->
  case Predecessor of
    nil ->
      {Nkey, Npid};

    {Pkey, _} ->
      case key:between(Nkey, Pkey, Id) of
        true ->
          {Nkey, Npid};
        false ->
          Predecessor
      end
  end.




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
