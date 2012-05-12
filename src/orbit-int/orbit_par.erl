-module(orbit_par).

-export([main/1]).
-export([small/0,medium/0,big/0]).

small() ->  {fun bench:g12/1,    100000}.
medium() -> {fun bench:g1234/1,  100000}.
big() ->    {fun bench:g2345/1, 1000000}.


main({Gen, N}) ->
    bench:par(Gen, N, erlang:system_info(schedulers)).
