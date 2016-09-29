-module(map).
-export([new/0]).
-export([update/3]).
-export([reachable/2]).
-export([all_nodes/1]).
%-export([remove_entry/2]).

% ---------------- [new/0] ----------------
new() -> [].

% ---------------- [update/3] ----------------
update(Node, Link, Map) ->
  IntMap = remove_entry(Node, Map),
  IntMap ++ [{Node, Link}].

% ---------------- [remove_entry/2] ----------------
remove_entry(_, []) ->
  [];
remove_entry(Node, [{Node, _}|T]) ->
  T;
remove_entry(Node, [H|T]) ->
  [H|remove_entry(Node, T)].

% ---------------- [reachable/2] ----------------
reachable(_, []) ->
  [];
reachable(Node, [{Node, R}|_]) ->
  R;
reachable(Node, Map) ->
  [_|T] = Map,
  reachable(Node, T).

% ---------------- [all_nodes/1] ----------------
all_nodes(Map) ->
  add_unique_from_map([], Map).

add_unique_from_map(CurrentList, []) ->
  CurrentList;

add_unique_from_map(CurrentList, Map) ->
  [Entry|SubMap] = Map,
  IntList = add_unique_from_map_entry(CurrentList, Entry),
  add_unique_from_map(IntList, SubMap).

% ---------------- [all_nodes/2] ----------------
add_unique_from_map_entry(CurrentList, MapEntry) ->
  {Node, List} = MapEntry,
  IntList = add_unique_from_node(CurrentList, Node),
  add_unique_from_list(IntList, List).

add_unique_from_node(CurrentList, Node) ->
  IntList = CurrentList -- [Node],
  IntList ++ [Node].

add_unique_from_list(CurrentList, []) ->
  CurrentList;
add_unique_from_list(CurrentList, List) ->
  [Node|SubList] = List,
  IntList = add_unique_from_node(CurrentList, Node),
  add_unique_from_list(IntList, SubList).


%Map:
%  []
%  [{Node, Link}, {Node, Link}]
%  [{berlin,[london,paris]}, {london,[berlin,paris]}]

%Node:
%  london
%  berlin
%  paris

%Link:
%  []
%  [london, paris]

insert(X,[]) ->
[X];
insert(X,Set) ->
case lists:member(X,Set) of
true  -> Set;
false -> [X|Set]
end.

Struggled with the [] at the end of functions until I removed nested lists
Struggled with adding elements with ++ and -- as I was adding them as elements and not as lists themselves (related to above)
