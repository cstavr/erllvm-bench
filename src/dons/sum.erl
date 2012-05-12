%% From: Smoking fast Haskell code using GHC?s new LLVM codegen
%% by donsbot

-module(sum).
-export([main/1, compile/1, sum/1]).

sum(N) ->
    lists:sum(lists:seq(1, N)).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,sum(10000000)).

main([]) ->
    loop(1,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
