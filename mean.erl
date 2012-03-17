%% From: Smoking fast Haskell code using GHC?s new LLVM codegen
%% by donsbot

-module(mean).
-export([test/0, compile/1, mean/1]).

mean(N) ->
    lists:sum(lists:duplicate(N, math:pi()))/N.

loop(0,R) -> R;
loop(N,_) -> loop(N-1,mean(100000000)).

test() ->
    T1 = bm:time_now(),
    _R = loop(3,0),
    Time = bm:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).
