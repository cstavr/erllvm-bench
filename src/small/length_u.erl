%% file: "length_u.erl"

-module(length_u).
-export([main/1,compile/1]).

len(L) -> len(0,L).

len(X,[_|[_|[_|[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]]]]) -> len(X+1,T);
len(X,[_|[_|[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]]]) -> len(X+1,T);
len(X,[_|[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]]) -> len(X+1,T);
len(X,[_|[_|[_|[_|[_|[_|[_|[_|T]]]]]]]]) -> len(X+1,T);
len(X,[_|[_|[_|[_|[_|[_|[_|T]]]]]]]) -> len(X+1,T);
len(X,[_|[_|[_|[_|[_|[_|T]]]]]]) -> len(X+1,T);
len(X,[_|[_|[_|[_|[_|T]]]]]) -> len(X+1,T);
len(X,[_|[_|[_|[_|T]]]]) -> len(X+1,T);
len(X,[_|[_|[_|T]]]) -> len(X+1,T);
len(X,[_|[_|T]]) -> len(X+1,T);
len(X,[_|T]) -> len(X+1,T);
len(X,[]) -> X.

make_list(X) -> make_list(X,[]).
make_list(0,L) -> L;
make_list(X,L) -> make_list(X-1,[0|L]).

loop(0,_,R) -> R;
loop(N,L,_) -> loop(N-1,L,len(L)).

main([]) ->
    L = make_list(20000),
    loop(50000,L,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
