-module(storage).

-export([create/0, add/3, lookup/2, split/3, merge/2]).

create() -> [].

add(Key, Value, Store) -> [{Key, Value} | Store].

lookup(_, []) -> false;
lookup(Key, [{Key, Value} | _]) -> {Key, Value};
lookup(Key, [_ | Rest]) -> lookup(Key, Rest).

split(From, To, Store) ->
  lists:partition(fun({Key, _}) -> key:between(Key, From, To) end, Store).

merge(Entries, Store) ->      % no duplicates
  lists:umerge(Store, Entries).
