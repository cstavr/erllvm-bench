-module(zip_nnc).
%% -export([main/1, compile/1, zip/1]).
-compile(export_all).

zip(N) ->
    sum(lists:map(fun (X) -> X bsl 1 end,
                        (lists:zipwith(fun (X, Y) -> X * Y end,
                                       seq(1, N),
                                       duplicate(N, 42))))).

sum(X) -> sum(X, 0).
sum([], Acc) -> Acc;
sum([X|Xs], Acc) -> sum(Xs, X+Acc).


seq(Start, End) ->
  seq(Start, End, []).

seq(End, End, Acc) -> reverse([End|Acc]);
seq(X, End, Acc) -> seq(X+1, End, [X|Acc]).

reverse(X) ->
  reverse(X, []).

reverse([], Acc) -> Acc;
reverse([X|Xs], Acc) -> reverse(Xs, [X|Acc]).

duplicate(N, X) ->
  duplicate(N, X, []).

duplicate(0, _, Acc) -> Acc;
duplicate(N, X, Acc) -> duplicate(N-1, X, [X|Acc]).


zipwith(Fun, A, B) ->
  zipwith(Fun, A, B, []).
zipwith(_Fun, [], [], Acc) -> reverse(Acc);
zipwith(Fun, [A|As], [B|Bs], Acc) ->
  zipwith(Fun, As, Bs,[Fun(A,B)|Acc]).


loop(0,R) -> R;
loop(N,_) -> loop(N-1,zip(10000000)).

main([]) ->
    loop(1,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
