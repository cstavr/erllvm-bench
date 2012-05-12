%% From: Smoking fast Haskell code using GHC's new LLVM codegen
%% by donsbot

-module(mean).
-export([main/1, compile/1, mean/1]).

mean(N) ->
    lists:sum(lists:duplicate(N, math:pi()))/N.

loop(0,R) -> R;
loop(N,_) -> loop(N-1,mean(10000000)).

main([]) ->
    loop(1,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
