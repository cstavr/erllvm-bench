% file: "nrev.erl"

-module(nrev).
-export([test/0,compile/1]).

nrev([H|T]) -> app(nrev(T),[H]);
nrev([])    -> [].

app([H|T],L) -> [H|app(T,L)];
app([],L)    -> L.

iota(N) -> iota(N,[]).
iota(0,L) -> L;
iota(N,L) -> iota(N-1,[N|L]).

loop(0,_,R) -> R;
loop(N,L,_) -> loop(N-1,L,nrev(L)).

test() ->
    T1 = run_benchmark:time_now(),
    L = iota(1000),
    _R = loop(1500,L,0),
    Time = run_benchmark:time_since(T1),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).
