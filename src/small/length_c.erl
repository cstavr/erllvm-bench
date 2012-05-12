%% file: "length_bif.erl"

-module(length_c).
-export([main/1,compile/1]).

len(L) -> length(L).

make_list(X) -> make_list(X,[]).
make_list(0,L) -> L;
make_list(X,L) -> make_list(X-1,[0|L]).

loop(0,_,R) -> R;
loop(N,L,_) -> loop(N-1,L,len(L)).

main([]) ->
    L = make_list(20000),
    _R = loop(50000,L,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
