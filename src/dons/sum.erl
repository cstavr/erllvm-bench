%% From: Smoking fast Haskell code using GHC?s new LLVM codegen
%% by donsbot

-module(sum).
-export([test/0, compile/1, sum/1]).

sum(N) ->
    lists:sum(lists:seq(1, N)).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,sum(10000000)).

test() ->
    T1 = run_benchmark:time_now(),
    _R = loop(1,0),
    Time = run_benchmark:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).
