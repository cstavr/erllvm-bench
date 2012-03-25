%%% The Computer Language Benchmarks Game
%%% http://shootout.alioth.debian.org/
%%% Contributed by Jiri Isa
%%% optimized run time options by shun shino

-module(threadring).
-export([main/1, roundtrip/3, test/0]).

-define(RING, 503).

%% Small, medium, big
-define(small, 500000).
-define(medium, 5000000).
-define(big, 50000000).

test() ->
    T1 = run_benchmark:time_now(),
    main([integer_to_list(?medium)]),
    run_benchmark:time_since(T1).

start(Dev, Token) ->
   H = lists:foldl(
      fun(Id, Pid) -> spawn(threadring, roundtrip, [Dev, Id, Pid]) end,
      self(),
      lists:seq(?RING, 2, -1)),
   H ! Token,
   roundtrip(Dev, 1, H).

roundtrip(Dev, Id, Pid) ->
   receive
      1 ->
         io:fwrite(Dev, "~b~n", [Id]),
         halt();
      Token ->
         Pid ! Token - 1,
         roundtrip(Dev, Id, Pid)
   end.

main([Arg]) ->
   Token = list_to_integer(Arg),
   {ok, Dev} = file:open("/dev/null", [write]),
   start(Dev, Token).
