%% file: "fib.erl"

-module(fib).
-export([test/0,compile/1,fib/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(X) -> fib(X-1) + fib(X-2).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,fib(34)).

test() ->
    T1 = bm:time_now(),
    _R = loop(30,0),
    Time = bm:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).
