-module(sum_nnc).
-export([main/0, compile/1, sum/1]).

sum(X) -> sum(X, 0).
sum([], Acc) -> Acc;
sum([X|Xs], Acc) -> sum(Xs, X+Acc).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,sum(10000000)).

main() ->
    T1 = run_benchmark:time_now(),
    _R = loop(1,0),
    Time = run_benchmark:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).
