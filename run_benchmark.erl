%% -*- erlang-indent-level: 2 -*-
-module(run_benchmark).
-export([run/1]).
-export([time_now/0, time_since/1]).

run([M, Mode]) ->
  bench_file(M, Mode).

bench_file(File, Mode) ->
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
  ok = compile(File, Mode),
  io:format("~w~n", [run_bench(File, cold_heap)]).

compile(_File, beam) ->
  ok;
compile(File, hipe) ->
  {ok, File} = hipe:c(File, [{regalloc,coalescing}, o2]),
  ok;
compile(File, erllvm) ->
  {ok, File} = hipe:c(File, [o2, to_llvm]),
  ok.

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
