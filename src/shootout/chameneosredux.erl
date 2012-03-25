%%% The Computer Language Benchmarks Game
%%% http://shootout.alioth.debian.org/
%%% contributed by Christian von Roques
%%% modified by Jiri Isa

%% Each chameneos is its own process.
%% A chameneos sends {self(), Color} to the broker to request a
%% meeting with another chameneos.
%% The broker replies with {Pid, Color} of the partner met or 'stop'
%% whereupon the chameneos prints the Meetings and Selfmeetings it had
%% and replies with the number of Meetings for the broker to sum.

-module(chameneosredux).
-export([main/1, test/0]).

-import(lists, [foreach/2]).

%% Small, medium, big
-define(small, 60000).
-define(medium, 600000).
-define(big, 6000000).

test() ->
    T1 = run_benchmark:time_now(),
    main([integer_to_list(?medium)]),
    run_benchmark:time_since(T1).

spell(0) -> " zero";
spell(N) -> spell(N, []).

spell(0, L) -> L;
spell(N, L) -> spell(N div 10, [element(N rem 10 + 1, {" zero", " one", " two", " three", " four", " five", " six", " seven", " eight", " nine"}) | L]).


complement(C, C) -> C;
complement(blue, red) -> yellow;
complement(blue, yellow) -> red;
complement(red, blue) -> yellow;
complement(red, yellow) -> blue;
complement(yellow, blue) -> red;
complement(yellow, red) -> blue.


show_complements(Dev) ->
    [ io:fwrite(Dev, "~p + ~p -> ~p~n", [A, B, complement(A, B)]) ||
        A <- [blue, red, yellow],
        B <- [blue, red, yellow]].


print_header(Dev, L) ->
    io:fwrite(Dev, "~n", []),
    foreach(fun(C) -> io:fwrite(Dev, " ~p", [C]) end, L),
    io:fwrite(Dev, "~n", []).


run(Dev, L, N) ->
    print_header(Dev, L),
    Broker = self(),
    foreach(fun(Color) -> spawn(fun() -> chameneos(Dev, Broker, Color, 0, 0) end) end, L),
    broker(N),
    cleanup(Dev, length(L), 0).


chameneos(Dev, Broker, Color, Meetings, MetSelf) ->
    Broker ! { self(), Color },
    receive
        {OPid, OColor} ->
            chameneos(Dev, Broker, complement(Color, OColor), Meetings+1,
                      if OPid == self() -> MetSelf+1; true -> MetSelf end);
        stop ->
            io:fwrite(Dev, "~w~s\n", [Meetings, spell(MetSelf)]),
            Broker ! Meetings
    end.


broker(0) -> nil;
broker(N) ->
    receive
        C1 = {Pid1, _} -> nil
    end,
    receive
        C2 = {Pid2, _} ->
            Pid1 ! C2,
            Pid2 ! C1,
            broker(N-1)
    end.

cleanup(Dev, 0, M) -> io:fwrite(Dev, "~s~n", [spell(M)]);
cleanup(Dev, N, M) ->
    receive
        {Pid, _Color} ->
            Pid ! stop,
            cleanup(Dev, N, M);
        Meetings ->
            cleanup(Dev, N-1, M+Meetings)
    end.


main([Arg]) ->
    N = list_to_integer(Arg),
    {ok, Dev} = file:open("/dev/null", [write]),
    show_complements(Dev),
    run(Dev, [blue, red, yellow], N),
    run(Dev, [blue, red, yellow, red, yellow, blue, red, yellow, red, blue], N),
    io:fwrite(Dev, "~n", []).
    %%halt(0).
