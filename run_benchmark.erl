%% -*- erlang-indent-level: 2 -*-
-module(run_benchmark).
-export([run/1]).
-export([time_now/0, time_since/1]).

run([M]) ->
  bench_file(M).

bench_file(File) ->
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
  Size = medium,
  ModExports = element(2, lists:keyfind(exports, 1, File:module_info())),
  Args =
    case lists:member({Size,0}, ModExports) of
      true -> [integer_to_list(File:medium())];
      false -> []
    end,
  spawn_opt(fun () ->
        % Supress IO
        {ok, F} = file:open("result_file", [write]),
        group_leader(F, self()),
        T1 = run_benchmark:time_now(),
        Time = try
          File:main(Args),
          run_benchmark:time_since(T1)
        catch
          exit:ok -> run_benchmark:time_since(T1);
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
  timer:now_diff(T2, T1)/1000. % Return millisecs
