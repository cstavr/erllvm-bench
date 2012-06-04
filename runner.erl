-module(runner).
-export([main/1]).

main([Bench, Platform, Number, Size]) ->
  Bench1 = list_to_atom(Bench),
  Number1 = list_to_integer(Number),
  Size1 = list_to_atom(Size),
  Size2 = Bench1:Size1(),
  Time = loop(Bench1, Number1, Size2, 0),
  Time1 = Time/Number1,
  ResFile = lists:concat(["results/runtime_", Platform, ".res"]),
  file:write_file(ResFile, io_lib:fwrite("~w ~.3f\n", [Bench1, Time1])
                  , [append]).

loop(_, 0, _, TotalTime) -> TotalTime;
loop(Bench, Number, Size, TotalTime) ->
  Run =
    fun () ->
        try
          Bench:main([integer_to_list(Size)])
        catch
          exit:ok -> ok;
          A:B ->
            erlang:display(A),
            erlang:display(B),
            throw(badexit)
        end
    end,
  {_, _} = erlang:statistics(runtime),
  Run(),
  {_, Time} = erlang:statistics(runtime),
  %% {Time, Value} = timer:tc(Run, []),
  %% Time1 = Time/1000000,
  Time1 = Time/1000,
  ResFile = lists:concat(["analytics/", atom_to_list(Bench)]),
  file:write_file(ResFile, io_lib:fwrite("~w ~.3f\n", [Bench, Time1])
                  , [append]),
  loop(Bench, Number-1, Size, TotalTime+Time1).


