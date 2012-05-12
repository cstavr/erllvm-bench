-module(orbit_seq).

-export([main/1]).
-export([small/0,medium/0,big/0]).

small() ->  {fun bench:g2345/1,  10000}.
medium() -> {fun bench:g124/1,  200000}.
big() ->    {fun bench:g1245/1,  10000}.


main({Gen, N}) ->
    bench:seq(Gen, N).
