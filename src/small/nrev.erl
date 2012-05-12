% file: "nrev.erl"

-module(nrev).
-export([main/1,compile/1]).

nrev([H|T]) -> app(nrev(T),[H]);
nrev([])    -> [].

app([H|T],L) -> [H|app(T,L)];
app([],L)    -> L.

iota(N) -> iota(N,[]).
iota(0,L) -> L;
iota(N,L) -> iota(N-1,[N|L]).

loop(0,_,R) -> R;
loop(N,L,_) -> loop(N-1,L,nrev(L)).

main([]) ->
    L = iota(1000),
    loop(1500,L,0).


compile(Flags) ->
    hipe:c(?MODULE,Flags).
