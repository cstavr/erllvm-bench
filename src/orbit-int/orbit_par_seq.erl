-module(orbit_par_seq).

-export([test/0]).

-define(small,  {fun bench:g12/1,    100000}).
-define(medium, {fun bench:g1234/1,  100000}).
-define(big,    {fun bench:g2345/1, 1000000}).

test() ->
    T1 = run_benchmark:time_now(),
    main(?medium),
    run_benchmark:time_since(T1).

main({Gen, N}) ->
    bench:par_seq(Gen, N, erlang:system_info(schedulers)).
