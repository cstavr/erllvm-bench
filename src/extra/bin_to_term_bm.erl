-module(bin_to_term_bm).
-export([test/0,t_binary_to_term/1]).

test() ->
    T1 = run_benchmark:time_now(),
    t_binary_to_term(1000000),
    Time = run_benchmark:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.


t_binary_to_term(Iter) ->
    Term = {a,{nested,tuple,is},nice,lists:seq(-1, 10),33,self()},
    t_binary_to_term(Iter, term_to_binary(Term), lists:duplicate(1024, 255)).

t_binary_to_term(0, _Bin, _T) -> ok;
t_binary_to_term(Iter, Bin, T) ->
    binary_to_term(Bin),
    t_binary_to_term(Iter-1, Bin, T).
