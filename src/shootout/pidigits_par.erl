%% The Computer Language Benchmarks Game
%% http://shootout.alioth.debian.org/
%%
%% Contributed by : Alkis Gotovos 10 Oct 2010

-module(pidigits_par).

-export([main/1]).
-export([small/0,medium/0,big/0]).

%% Small, medium, big
small() -> 10.
medium() -> 6000.
big() -> 10000.


main(N) when is_integer(N) ->
    Pid = spawn_link(fun() -> io_worker() end),
    register(io_worker, Pid),
    stream({1, 0, 1}, 1, 0, N).

comp({Q, R, T}, {U, V, X}) -> {Q*U, Q*V + R*X, T*X}.

next({Q, R, T}) -> (Q*3 + R) div T.

safe({Q, R, T}, N) -> N == (Q*4 + R) div T.

prod({Z11, Z12, Z22}, N) -> {10*Z11, 10*(Z12 - N*Z22), Z22}.

stream(Z, K, P, N) ->
    Y = next(Z),
    case safe(Z, Y) of
        true ->
            io_worker ! {Y, P + 1, N},
            stream(prod(Z, Y), K, P + 1, N);
        false -> stream(comp(Z, {K, 4*K + 2, 2*K + 1}), K + 1, P, N)
    end.

io_worker() ->
    receive
        {_Y, N, N} ->
            Spaces = (10 - N rem 10) rem 10,
            Spaces,
            %%io:fwrite("~w~.*c\t:~w~n", [_Y, Spaces, $ , N]),
            erlang:halt(0);
        {_Y, P, _N} when P rem 10 == 0 ->
            %%io:fwrite("~w\t:~w~n", [_Y, P]),
            io_worker();
        {_Y, _P, _N} ->
            %%io:fwrite("~w", [_Y]),
            io_worker()
    end.
