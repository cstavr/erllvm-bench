%% From: Smoking fast Haskell code using GHC?s new LLVM codegen
%% by donsbot

-module(zip3).
-export([test/0, compile/1, zip3/1]).

zip3(N) ->
    lists:sum(lists:zipwith3(fun (X, Y, Z) -> X * Y * Z end,
                             lists:seq(1, N),
                             lists:seq(2, N+1),
                             lists:seq(7, N+6))).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,zip3(10000000)).

test() ->
    T1 = run_benchmark:time_now(),
    _R = loop(1,0),
    Time = run_benchmark:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).
