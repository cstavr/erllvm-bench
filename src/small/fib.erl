%% file: "fib.erl"

-module(fib).
-export([main/1,compile/1,fib/1]).

fib(0) -> 0;
fib(1) -> 1;
fib(X) -> fib(X-1) + fib(X-2).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,fib(34)).

main([]) ->
    loop(30,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
