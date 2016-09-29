-module(rudy).                            % load rudy module

-export([init/1]).
-export([handler/1]).
-export([request/1]).
-export([reply/1]).
-export([start/1, stop/0]).

init(Port) ->
  Opt = [list, {active, false}, {reuseaddr, true}],
  case gen_tcp:listen(Port, Opt) of % http://erlang.org/doc/man/gen_tcp.html
    {ok, Listen} ->
      handler(Listen),              % pass the socket to the handler
      gen_tcp:close(Listen),        % finally close Listen socket
      ok;
    {error, Error} ->
      error
  end.

handler(Listen) ->
  case gen_tcp:accept(Listen) of    % http://erlang.org/doc/man/gen_tcp.html
    {ok, Client} ->
      request(Client),              % pass the Client connection to request
      handler(Listen);              % call itself recursively so it doesn't terminate after one request
    {error, Error} ->
      error
  end.

request(Client) ->
  Recv = gen_tcp:recv(Client, 0),    % once we have connection to Client, we read input and return it as string, 0 reads as much as possible
  case Recv of
    {ok, Str} ->
      Request = http:parse_request(Str),  % parse Request
      Response = reply(Request),
      gen_tcp:send(Client, Response);   % send back reply, in form of string, to Client
    {error, Error} ->
      io:format("rudy: error: ~w~n", [Error])
  end,
  gen_tcp:close(Client).          % close Client socket

reply({{get, URI, _}, _, _}) ->
  %timer:sleep(40),
  http:ok("Hello Erlang!").

start(Port) ->
  register(rudy, spawn(fun() -> init(Port) end)).

stop() ->
  exit(whereis(rudy), "time to die").
