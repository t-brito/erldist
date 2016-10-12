-module(key).

-export([generate/0, between/3]).

generate() -> rand:uniform(1000000000).

between(_, From, To) when From == To -> true;                         % full circle
between(Key, From, To) when Key > From, Key =< To -> true;            % normal case
between(Key, From, To) when From > To -> Key > From orelse Key =< To;
between(_, _, _) -> false.
