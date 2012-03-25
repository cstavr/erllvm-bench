%   The Computer Language Benchmarks Game
%   http://shootout.alioth.debian.org/
%   contributed by Fredrik Svahn

-module(spectralnorm_par).
-export([main/1, test/0]).
-compile( [ inline, { inline_size, 1000 } ] ).

%% Small, medium, big
-define(small, 500).
-define(medium, 3000).
-define(big, 5500).

test() ->
    T1 = run_benchmark:time_now(),
    main([integer_to_list(?medium)]),
    run_benchmark:time_since(T1).

main([Arg]) ->
    %%register(server, self()),
    {ok, Dev} = file:open("/dev/null", [write]),
    N = list_to_integer(Arg),
    {U, V} = power_method(self(), N, 10, erlang:make_tuple(N, 1), []),
    io:format(Dev, "~.9f\n", [ eigen(N, U, V, 0, 0) ]).
    %%erlang:halt(0).

% eigenvalue of V
eigen(0, _, _, VBV, VV) when VV /= 0 -> math:sqrt(VBV / VV);

eigen(I, U, V, VBV, VV) when I /= 0 ->
    VI = element(I, V),
    eigen(I-1, U, V, VBV + element(I, U)*VI, VV + VI*VI).

% 2I steps of the power method
power_method(_Server, _, 0, A, B) -> {A, B};
power_method(Server, N, I, A, _B) ->
    V = atav(Server, N, A),
    U = atav(Server, N, V),
    power_method(Server, N, I-1, U, V).


% return element i,j of infinite matrix A
a(II,JJ) -> 1/((II+JJ-2)*(II-1+JJ)/2+II).


% multiply vector v by matrix A
av(Server, N, V) -> pmap(N, fun(Begin, End) -> av(Server, N, Begin, End, V) end).

av(Server, N, Begin, End, V) -> Server ! { self(), [ avloop(N, I, V, 0.0) || I <- lists:seq(Begin, End) ]}.

avloop(0, _, _, X) ->  X;
avloop(J, I, V, X) ->  avloop(J-1, I, V, X + a(I, J)*element(J, V) ).


% multiply vector v by matrix A transposed
atv(Server, N, V) -> pmap(N, fun(Begin, End)-> atv(Server, N, Begin, End, V) end).

atv(Server, N, Begin, End, V) -> Server ! { self(), [ atvloop(N, I, V, 0.0) || I <- lists:seq(Begin, End) ]}.

atvloop(0, _, _, X) -> X;
atvloop(J, I, V, X) -> atvloop(J-1, I, V, X + a(J, I)*element(J, V) ).


% multiply vector v by matrix A and then by matrix A transposed
atav(Server, N, V) -> atv(Server, N, av(Server, N, V)).


%Helper function for multicore
pmap(N, F) ->
    Chunks = chunks(0, erlang:system_info(logical_processors), N, []),
    Pids = [spawn(fun()-> F(Begin, End) end) || {Begin, End} <- Chunks],
    Res = [ receive {Pid, X} -> X end || Pid <- Pids],
    list_to_tuple(lists:flatten(Res)).

chunks(I, P, N, A) when I == P-1 -> lists:reverse([{I*(N div P)+1, N} | A ]);
chunks(I, P, N, A) -> chunks(I+1, P, N, [{ I*(N div P)+1, (I+1)*(N div P)} | A ]).

