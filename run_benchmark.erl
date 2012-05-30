%% -*- erlang-indent-level: 2 -*-
-module(run_benchmark).

-export([run/1]).

-include("stats.hrl").

run([Module, Comp, N, Type]) ->
  case Type of
    erjang -> bench_erjang_file(Module, Comp,
        list_to_integer(atom_to_list(N)));
    _ ->
  bench_file(Module, Comp, list_to_integer(atom_to_list(N)), Type)
end.

bench_file(File, Comp, N, Type) ->
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
  T = run_bench(File, N),
  %% Write results/errors to files:
  ResFile = lists:concat(["results/runtime_", Comp, ".res"]),
  file:write_file(ResFile, io_lib:fwrite("~w\t~.3f\n", [File, T#stat.median])
                  , [append]),
  ErrFile = lists:concat(["results/runtime_", Comp, "-err.res"]),
  file:write_file(ErrFile, io_lib:fwrite("~w\t~.3f\n", [File, T#stat.stddev])
                  , [append]).

bench_erjang_file(File, Comp, N) ->
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
  %% Warm Up Erjang
  _ = warm_up(File, 4, 4),
  T = run_bench(File, 2),
  ResFile = lists:concat(["foo"]),
  file:write_file(ResFile, io_lib:fwrite("~w\t~.3f\n", [File, T#stat.median])
                  , [append]),
  os:cmd("cat foo >> results/runtime_erjang.res").

run_bench(File, N) when is_integer(N) ->
  Myself = self(),
  Opts = [], %[{min_heap_size, 100000000}],
  Size = big,
  ModExports = element(2, lists:keyfind(exports, 1, File:module_info())),
  Args =
    case lists:member({Size,0}, ModExports) of
      true -> File:big();
      false -> []
    end,
  spawn_opt(fun () ->
                %% Supress IO
                {ok, F} = file:open("io_file", [write]),
                group_leader(F, self()),
                %% Use a runner in order to catch the exiting exception.
                Runner = fun () -> try
                                     File:main(Args)
                                   catch
                                     exit:ok -> ok;
                                     _:_ -> badexit
                                   end
                         end,
                Times = stats:test_avg(Runner, [], N),
                Myself ! Times,
                file:close(F)
            end, Opts),
  receive
    Result -> Result
  end.


warm_up(File, 0, M) -> ok;
warm_up(File, N, M) ->
  T = run_bench(File, 1),
  ResFile = "baz",
  file:write_file(ResFile, io_lib:fwrite("~w\t~.3f\n", [File, T#stat.median])
                  , [append]),
  case N of
    M -> os:cmd("cat baz >> results/runtime_erjang_1.res");
    _ -> ok
  end,
  os:cmd("cat baz >> results/runtime_erjang.analytics"),
  timer:sleep(1),
  warm_up(File, N-1, M).

