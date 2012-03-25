% The Computer Language Shootout
% http://shootout.alioth.debian.org/
%
% contributed by Isaac Gouy (Erlang novice)

-module(binarytrees).
-export([main/1, test/0]).

-define(Min,4).

%% Small, medium, big
-define(small, 12).
-define(medium, 16).
-define(big, 20).

test() ->
    T1 = run_benchmark:time_now(),
    main([integer_to_list(?medium)]),
    run_benchmark:time_since(T1).

main([Arg]) ->
   N = list_to_integer(Arg),
   {ok, Dev} = file:open("/dev/null", [write]),
   Max = lists:max([?Min+2,N]),

   Stretch = Max + 1,
   io:fwrite(Dev, "stretch tree of depth ~w\t check: ~w~n",
      [ Stretch, itemCheck(bottomUp(0,Stretch)) ]),

   LongLivedTree = bottomUp(0,Max),
   depthLoop(Dev, ?Min,Max),

   io:fwrite(Dev, "long lived tree of depth ~w\t check: ~w~n",
      [ Max, itemCheck(LongLivedTree) ]).
   %%halt(0).


depthLoop(_Dev, D,M) when D > M -> ok;
depthLoop(Dev, D,M) ->
   N = 1 bsl (M-D + ?Min),
   io:fwrite(Dev, "~w\t trees of depth ~w\t check: ~w~n",
      [ 2*N, D, sumLoop(N,D,0) ]),
   depthLoop (Dev, D+2,M).

sumLoop(0,_,Sum) -> Sum;
sumLoop(N,D,Sum) ->
   sumLoop(N-1,D, Sum + itemCheck(bottomUp(N,D)) + itemCheck(bottomUp(-1*N,D))).

bottomUp(I,0) -> {I, nil, nil};
bottomUp(I,D) -> {I, bottomUp(2*I-1,D-1), bottomUp(2*I,D-1)}.

itemCheck(nil) -> 0;
itemCheck({I,Left,Right}) ->
   I + itemCheck(Left) - itemCheck(Right).
