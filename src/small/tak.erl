%% file: "tak.erl"

-module(tak).
-export([main/1,compile/1,tak/3]).

tak(X,Y,Z) ->
  if
    Y<X -> tak( tak(X-1,Y,Z),
                tak(Y-1,Z,X),
                tak(Z-1,X,Y) );
    true -> Z
  end.

loop(0,R) -> R;
loop(N,_) -> loop(N-1,tak(32,22,16)).

main([]) ->
    loop(1000,0).

compile(Flags) ->
    hipe:c(?MODULE,Flags).
