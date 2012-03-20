%% -*- erlang-indent-level: 2 -*-
-module(run_benchmark).
-export([run/1]).
-export([time_now/0, time_since/1]).

run([M]) ->
  {ok, Dev} = file:open("results/runtime.res", [append]),
  bench_file(Dev, M),
  file:close(Dev).

bench_file(Dev, File) ->
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
  io:format(Dev,"~-16w &", [File]),
  %% BEAM
  BT = run_bench(File, cold_heap),
  io:format(Dev," ~6.2f &",[BT/1000]),
  %% HiPE
  hipe:c(File,[{regalloc,coalescing}, o2]),
  HT = run_bench(File, cold_heap),
  io:format(Dev," ~6.2f &",[HT/1000]),
  %% LLVM
  hipe:c(File,[o2,to_llvm]),
  LT = run_bench(File, cold_heap),
  io:format(Dev," ~6.2f &",[LT/1000]),
  %% Speed Ups
  case File of
    w_estone -> io:format(Dev," ~6.2f & ~6.2f \\\\\n", [LT/BT, LT/HT]);
    _        -> io:format(Dev," ~6.2f & ~6.2f \\\\\n", [BT/LT, HT/LT])
  end.

run_bench(File, cold_heap) ->
  garbage_collect(),
  File:test();
run_bench(File, warm_heap) ->
  File:test().

time_now() ->
  statistics(runtime).

time_since(_) ->
  {_, T2} = statistics(runtime),
  T2.
