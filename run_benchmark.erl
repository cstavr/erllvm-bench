%% -*- erlang-indent-level: 2 -*-
-module(run_benchmark).
-export([run/1]).

run([Module, Comp]) ->
  bench_file(Module, Comp).

bench_file(File, Comp) ->
  case File of
    prettypr ->
      case get(prettypr_data) of
	undefined -> {ok,[X]} =
	  file:consult("prettypr.input"),
	  put(prettypr_data, X),
	  ok;
	_ -> ok
      end;
    _ -> ok
  end,
  T = run_bench(File),
  %% Write results/errors to files:
  ResFile = lists:concat(["results/runtime_", Comp, ".res"]),
  file:write_file(ResFile, io_lib:fwrite("~w\t~.3f\n", [File, T])
                  , [append]),
  ErrFile = lists:concat(["results/runtime_", Comp, "-err.res"]),
  file:write_file(ErrFile, io_lib:fwrite("~w\t~.3f\n", [File, T]) %FIXME: Err!
                  , [append]).

run_bench(File) ->
  Myself = self(),
  Opts = [], %[{min_heap_size, 100000000}],
  Size = medium,
  ModExports = element(2, lists:keyfind(exports, 1, File:module_info())),
  Args =
    case lists:member({Size,0}, ModExports) of
      true -> File:medium();
      false -> []
    end,
  spawn_opt(fun () ->
        % Supress IO
        {ok, F} = file:open("io_file", [write]),
        group_leader(F, self()),
        T1 = time_now(),
        Time = try
                 File:main(Args),
                 time_since(T1)
               catch
                 exit:ok -> time_since(T1);
                 _:_ -> -1
               end,
        Myself ! Time,
        file:close(F)
        end, Opts),
  receive
    Result -> Result
  end.

time_now() ->
  erlang:now().

time_since(T1) ->
  T2 = erlang:now(),
  timer:now_diff(T2, T1)/1000000. % Return seconds
