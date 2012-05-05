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
  io:format("~w~n", [run_bench(File)]).

run_bench(File) ->
  Myself = self(),
  Opts = [], %[{min_heap_size, 100000000}],
  spawn_opt(fun () -> Myself ! File:test() end, Opts),
  receive
    Result -> Result
  end.

time_now() ->
  erlang:now().

time_since(T1) ->
  T2 = erlang:now(),
  timer:now_diff(T2, T1)/1000. % Return millisecs
