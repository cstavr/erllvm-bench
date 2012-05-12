-module(sum_nnc).
-export([main/1, compile/1, sum/1]).

sum(X) -> sum(X, 0).
sum([], Acc) -> Acc;
sum([X|Xs], Acc) -> sum(Xs, X+Acc).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,sum(10000000)).

main([]) ->
    loop(1,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
