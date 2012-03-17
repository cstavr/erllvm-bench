%% -*- erlang-indent-level: 2 -*-
%%=================================================================
%% File: bm.erl
%%
%% For each of the files which are specified in run/0, displays:
%%   - BEAM execution time
%%   - HiPE execution time
%%   - LLvM exceution time
%%   - Speedups
%%=================================================================

-module(bm).
-export([run/0,time_now/0,time_since/1]).

run() ->
  {ok,Dev} = file:open("results/runtime.res", [write]),
  Files =
    [fib,tak,length_c,length,length_u,qsort,smith,huff,decode,
    ring,life,barnes,yaws_html,prettypr,nrev,stable],
  Extra =
    [nrev,pseudoknot, float_bm,fun_bm,freq_bm,call_tail_bm,call_bm,
     bs_sum_bm,bs_simple_bm,bs_bm,bin_to_term_bm],
  Loops =
    [sum, zip3, mean],

  bench(Dev, Loops),
  bench(Dev, Files),
  bench_file(Dev, w_estone),
  bench(Dev, Extra),
  file:close(Dev),
  halt().

bench(_, []) -> ok;
bench(Dev, [File|Files]) ->
  bench_file(Dev, File),
  bench(Dev, Files).

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
  BT = bench_on_clean_heap(File),
  io:format(Dev," ~6.2f &",[BT/1000]),
  %% HiPE
  hipe:c(File,[{regalloc,coalescing}, o2]),
  HT = bench_on_clean_heap(File),
  io:format(Dev," ~6.2f &",[HT/1000]),
  %% LLVM
  hipe:c(File,[o2,to_llvm]),
  LT = bench_on_clean_heap(File),
  io:format(Dev," ~6.2f &",[LT/1000]),
  %% Speed Ups
  case File of
    w_estone -> io:format(Dev," ~6.2f & ~6.2f \\\\\n", [LT/BT, LT/HT]);
    _        -> io:format(Dev," ~6.2f & ~6.2f \\\\\n", [BT/LT, HT/LT])
  end.

bench_on_clean_heap(File) ->
  garbage_collect(),
  File:test().

time_now() ->
  statistics(runtime).

time_since(_) ->
  {_, T2} = statistics(runtime),
  T2.
