% The Computer Language Benchmarks Game
% http://shootout.alioth.debian.org/
% contributed by Fredrik Svahn

-module(fasta).
-export([main/1, test/0]).

-define(LINELEN, 60).
-define(PREC,10000000).

-define(IM, 139968).
-define(IC, 29573).
-define(IA, 3877).

-define(ALU,<<"GGCCGGGCGCGGTGGCTCACGCCTGTAATCCCAGCACTTTGGGAGGCCGAGGCGGGCGGATCACCTGAGGTCAGGAGTTCGAGACCAGCCTGGCCAACATGGTGAAACCCCGTCTCTACTAAAAATACAAAAATTAGCCGGGCGTGGTGGCGCGCGCCTGTAATCCCAGCTACTCGGGAGGCTGAGGCAGGAGAATCGCTTGAACCCGGGAGGCGGAGGTTGCAGTGAGCCGAGATCGCGCCACTGCACTCCAGCCTGGGCGACAGAGCGAGACTCCGTCTCAAAAA">>).

-define(IUB, [{$a, 0.27}, {$c, 0.12}, {$g, 0.12}, {$t, 0.27}, {$B, 0.02}, {$D, 0.02}, {$H, 0.02}, {$K, 0.02}, {$M, 0.02}, {$N, 0.02}, {$R, 0.02}, {$S, 0.02}, {$V, 0.02}, {$W, 0.02}, {$Y, 0.02}]).

-define(HS, [{$a, 0.3029549426680}, {$c, 0.1979883004921}, {$g, 0.1975473066391}, {$t, 0.3015094502008}]).

%% Small, medium, big
-define(small, 250000).
-define(medium, 2500000).
-define(big, 25000000).

test() ->
    T1 = run_benchmark:time_now(),
    main([integer_to_list(?medium)]),
    run_benchmark:time_since(T1).

main([Arg]) ->
    %%put(port, open_port({fd,0,1}, [out, binary])),
    {ok, Dev} = file:open("/dev/null", [write, binary]),
    Seed = 42,
    N = list_to_integer(Arg),

    print(Dev, <<">ONE Homo sapiens alu\n">>),
    cycle(Dev, ?ALU, N*2, [], 0),

    print(Dev, <<">TWO IUB ambiguity codes\n">>),
    NewSeed = rand(Dev, mk_list(?IUB), ?LINELEN, N*3, [], Seed, [], 0),

    print(Dev, <<">THREE Homo sapiens frequency\n">>),
    rand(Dev, mk_list(?HS), ?LINELEN, N*5, [], NewSeed, [],0).
    %%halt(0).

%Newline every LINELEN char, however io is expensive so we want to buffer
%up a few lines before printing. 16 lines in buffer seem to be fastest.
cycle(Dev, Seq, Total, RowBuf, _) when Total < ?LINELEN ->
    <<Seq1:Total/binary, _/binary>> = <<Seq/binary, ?ALU/binary>>,
    reverse_print(Dev, RowBuf),
    print(Dev, <<Seq1/binary, "\n">>);

cycle(Dev, Seq, Total, RowBuf, RowBufSize) when RowBufSize == 16 ->
    reverse_print(Dev, RowBuf),
    cycle(Dev, Seq, Total, [], 0);

cycle(Dev, Seq, Total, RowBuf, RowBufSize ) when size(Seq) < ?LINELEN ->
    <<Seq1:?LINELEN/binary, Seq2/binary>> = <<Seq/binary, ?ALU/binary>>,
    cycle(Dev, Seq2, Total-?LINELEN, [<<Seq1/binary,"\n">>| RowBuf], RowBufSize+1);

cycle(Dev, Seq, Total, RowBuf, RowBufSize) ->
    <<Seq1:?LINELEN/binary, Seq2/binary>> = Seq,
    cycle(Dev, Seq2, Total-?LINELEN, [<<Seq1/binary,"\n">>| RowBuf], RowBufSize+1).

rand(Dev, _, _, 0, List, Seed, RowBuf, _) ->
    LastLine = lists:reverse(["\n" | List]),
    reverse_print(Dev, [LastLine | RowBuf]),
    Seed;

rand(Dev, Freq, 0, Total, List, Seed, RowBuf, RowBufSize) when RowBufSize == 16 ->
    Line = lists:reverse(["\n" | List]),
    reverse_print(Dev, [Line | RowBuf]),
    rand(Dev, Freq, ?LINELEN, Total, [], Seed, [], 0);

rand(Dev, Freq, 0, Total, List, Seed, RowBuf, RowBufSize) ->
    Line = lists:reverse(["\n" | List]),
    rand(Dev, Freq, ?LINELEN, Total, [], Seed, [ Line | RowBuf], RowBufSize + 1);

rand(Dev, Freq, LineLen, Total, List, Seed, RowBuf, RowBufSize) ->
    {Rand, NewSeed} = random(Seed),
    Base = get_base(Freq, Rand),
    rand(Dev, Freq, LineLen-1, Total-1, [Base | List], NewSeed, RowBuf, RowBufSize).

random(Seed) ->
    NewSeed = (Seed * ?IA + ?IC) rem ?IM,
    {trunc(NewSeed / ?IM * ?PREC), NewSeed}.

get_base([{Base, _}], _P) -> Base;
get_base([{Base, Freq}|_], P) when P < Freq -> Base;
get_base([{_, _} | Rest], P) -> get_base(Rest, P).

%Floats are expensive and we want to avoid dealing with floats in get_base/1.
%Precalculate list of accumulated integers
mk_list(Probs)-> lists:reverse(mk_list(Probs, 0, [])).
mk_list([{B, P}], AccP, AccL)-> [{B, AccP + trunc(P*?PREC)}| AccL];
mk_list([{B, P}|T], AccP, AccL)->
    mk_list(T, AccP + trunc(P*?PREC), [{B, AccP + trunc(P*?PREC)}| AccL]).

print(Dev, List) ->
    io:fwrite(Dev, "~w~n", [List]).
    %%port_command(get(port), List).
reverse_print(Dev, List) ->
    io:fwrite(Dev, "~w~n", [lists:reverse(List)]).
    %%port_command(get(port), lists:reverse(List)).
