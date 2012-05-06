%% The Great Computer Language Shootout 
%% http://shootout.alioth.debian.org/
%% 
%% modified by Isaac Gouy

-module(random).
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 12.
medium() -> 16.
big() -> 20.

main() -> main(["1"]).
main([Arg]) ->
   N = list_to_integer(Arg),
   io:fwrite("~.9f\n", [rand(N, 42, 0.0, 100.0)]),
   exit(ok).

-define(IM, 139968).
-define(IA, 3877).
-define(IC, 29573).

rand(0, _, Rand, _) -> Rand;
rand(N, Seed, Rand, Max) ->
   NewSeed = (Seed * ?IA + ?IC) rem ?IM,
   NewRand = Max * NewSeed / ?IM,
   rand(N-1, NewSeed, NewRand, Max).
