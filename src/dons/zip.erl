%% From: Smoking fast Haskell code using GHC?s new LLVM codegen
%% by donsbot

-module(zip).
-export([main/0, compile/1, zip/1]).

zip(N) ->
    lists:sum(lists:map(fun (X) -> X bsl 1 end,
                        (lists:zipwith(fun (X, Y) -> X * Y end,
                                       lists:seq(1, N),
                                       lists:duplicate(N, 42))))).

loop(0,R) -> R;
loop(N,_) -> loop(N-1,zip(10000000)).

main() ->
    T1 = run_benchmark:time_now(),
    _R = loop(1,0),
    Time = run_benchmark:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).
