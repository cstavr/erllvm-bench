%  The Great Computer Language Shootout
%   http://shootout.alioth.debian.org/
%
%   contributed by Mark Scandariato
%
%   erl -noshell -noinput -run pidigits main 7


-module(pidigits).
-export([main/1, test/0]).

%% Small, medium, big
-define(small, 2000).
-define(medium, 6000).
-define(big, 10000).

test() ->
    T1 = run_benchmark:time_now(),
    main([integer_to_list(?medium)]),
    run_benchmark:time_since(T1).

% conversion
is_safe(Z, N) -> N == extr(Z, 4).
next(Z)       -> extr(Z, 3).
prod(Z, N)    -> comp({10, -10*N, 0, 1}, Z).
cons(Z, Zp)   -> comp(Z, Zp).

% LFT
-define(unit, {1,0,0,1}).
comp({Q,R,S,T}, {Qp, Rp, Sp, Tp}) ->
    {Q*Qp + R*Sp, Q*Rp + R*Tp, S*Qp + T*Sp, S*Rp + T*Tp}.
extr({Q,R,S,T}, X) -> (Q * X + R) div (S * X + T).

lft(K) -> {K, 4*K+2, 0, 2*K+1}.

stream(Dev, N) -> stream(Dev, N, 0, 1, ?unit, []).
stream(Dev, N, N, _, _, P) -> print(Dev, N,P);
stream(Dev, N, C, K, Z, P) ->
    Y = next(Z),
    case is_safe(Z, Y) of
        true  ->
            stream(Dev, N, C+1, K, prod(Z,Y), update(Dev, C,Y,P));
        false ->
            stream(Dev, N, C, K+1, cons(Z, lft(K)), P)
    end.


update(Dev, C, D, P) when C rem 10 == 0, C > 0 ->
    print(Dev, C, P),
    [D];

update(_Dev, _, D, P) -> [D|P].


print(Dev, C, P) -> do_print(Dev, C, lists:reverse(P)).


do_print(Dev, C, []) when C rem 10 == 0 -> io:fwrite(Dev, "\t:~p~n", [C]);
do_print(Dev, C, []) -> io:fwrite(Dev, "~*.1c:~p~n", [10 - C rem 10, $\t, C]);
do_print(Dev, C, [H|T]) -> io:fwrite(Dev, "~p", [H]), do_print(Dev, C, T).


main([Arg]) ->
    N = list_to_integer(Arg),
    main(N);
    %%erlang:halt(0);

main(N) when N > 1 ->
    {ok, Dev} = file:open("/dev/null", [write]),
    stream(Dev, N).
