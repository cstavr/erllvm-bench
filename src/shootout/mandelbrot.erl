%% The Computer Language Benchmarks Game
%% http://shootout.alioth.debian.org/
%% Contributed by Fredrik Svahn based on Per Gustafsson's mandelbrot program

-module(mandelbrot).
-export([main/1, test/0]).
-define(LIM_SQR, 4.0).
-define(ITER, 50).
-define(SR, -1.5).
-define(SI, -1).

%% Small, medium, big
-define(small, 1000).
-define(medium, 4000).
-define(big, 16000).

test() ->
    T1 = run_benchmark:time_now(),
    main([integer_to_list(?medium)]),
    run_benchmark:time_since(T1).

main([Arg]) ->
    N = list_to_integer(Arg),
    {ok, Dev} = file:open("/dev/null", [write]),
    io:put_chars(Dev, ["P4\n", Arg, " ", Arg, "\n"]),

    %% Spawn one process per row
    Row = fun(Y)-> spawn(fun()-> row(Dev, 0, ?SI+Y*2/N, N, 0, [], 7) end) end,
    Pids = lists:map(Row, lists:seq(0,N-1)),

    %Pass token around to make sure printouts are in the right order
    hd(Pids) ! tl(Pids) ++ [ self() ],
    receive _Token -> ok end.

%Iterate over a row, collect bits, bytes and finally print the row
row(Dev, X, _, N, Bits, Bytes, BitC) when X =:= N-1 ->
    receive Pids ->
            put_chars(Dev, Bits, Bytes, BitC),
            hd(Pids) ! tl(Pids)
    end;

row(Dev, X, Y2, N, Bits, Bytes, 0) ->
    row(Dev, X+1, Y2, N, 0, [Bits bsl 1 + m(?ITER, ?SR+X*2/N, Y2) | Bytes], 7);

row(Dev, X, Y2, N, Bits, Bytes, BitC) ->
    row(Dev, X+1, Y2, N, Bits bsl 1 + m(?ITER, ?SR+X*2/N, Y2), Bytes, BitC-1).

%Mandelbrot algorithm
m(Iter, CR,CI) -> m(Iter - 1, CR, CI, CR, CI).

m(Iter, R, I, CR, CI) ->
    case R*R+I*I > ?LIM_SQR of
        false when Iter > 0 -> m(Iter-1, R*R-I*I+CR, 2*R*I+CI, CR, CI);
        false -> 1;
        true -> 0
    end.

put_chars(Dev, _, Bytes, 7)->
    io:put_chars(Dev, lists:reverse(Bytes));
put_chars(Dev, Bits, Bytes, C) ->
    io:put_chars(Dev, lists:reverse([Bits bsl (C+1) | Bytes])).
