-module(orbit_seq).

-export([test/0]).

-define(small,  {fun bench:g2345/1,  10000}).
-define(medium, {fun bench:g124/1,  200000}).
-define(big,    {fun bench:g1245/1,  10000}).

test() ->
    T1 = run_benchmark:time_now(),
    main(?medium),
    run_benchmark:time_since(T1).

main({Gen, N}) ->
    bench:seq(Gen, N).
