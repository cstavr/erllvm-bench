-module(orbit).

-export([test/0]).

-define(small, {fun bench:g1/1, 1000000}).
-define(medium, {fun bench:g124/1, 50000}).
-define(big, {fun bench:g1245/1, 100000}).

benchmark_arguments() ->
    [{Gen, N} || Gen <- [g1, g13, g124, g1245, g12345],
		 N <- [50000, 100000, 1000000]].

test() ->
    T1 = run_benchmark:time_now(),
    main(?medium),
    run_benchmark:time_since(T1).

main({Gen, N}) ->
    bench:seq(Gen, N).
