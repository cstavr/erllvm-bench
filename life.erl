-module(life).
-export([test/0,compile/1,cell/0]). 

test() ->
    T1 = bm:time_now(),
    _R = 0 = bench(30,30,10000),
    Time = bm:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

compile(Flags) ->
    hipe:c(?MODULE,Flags).

%%
%% start 
%%

bench(N,M,Num) ->
%    life_random:seed(),
    Raw = make(N,M),
    start_all(Raw,Num),
    sum_all(Raw).

%%
%% make matrix
%%

make(N,M) ->
    Raw = make_lines(N,M,[]),
    Matrix = complete(Raw),
    link_matrix(Matrix),
    Raw.

%%
%% make random initial configuration
%%

make_lines(0,_M,L) ->
    L;
make_lines(N,M,L) ->
    make_lines(N-1,M,[make_col(M) | L]).

make_col(0) ->
    [];
make_col(M) ->
    [spawn(?MODULE, cell, []) | make_col(M-1)].

%%
%% link neighbours
%%

link_matrix([_,_]) ->
    ok;
link_matrix([North | Rest]) ->
    [This, South | _] = Rest,
    link_line(North, This, South),
    link_matrix(Rest).

link_line([_,_], _, _) ->
    ok;
link_line([NW | RestN], [W | RestW], [SW | RestS]) ->
    [N, NE | _] = RestN,
    [This, E | _] = RestW,
    [S, SE | _ ] = RestS,
    This ! {neighbours, [NW,N,NE,W,E,SW,S,SE]},
    link_line(RestN, RestW, RestS).

%%
%% start reproduction of all cells
%%

start_all([],_N) ->
    true;
start_all([L|Ls], N) ->
    start_line(L, N),
    start_all(Ls, N).

start_line([],_N) ->
    true;
start_line([X|Xs], N) ->
    X ! {go, self(), 1, N},
    start_line(Xs, N).


sum_all([]) ->
    0;
sum_all([L|Ls]) ->
    sum_line(L) + sum_all(Ls).

sum_line([]) ->
    0;
sum_line([X|Xs]) ->
    receive
	{X, Last} ->
	   continue
    end,
    Last + sum_line(Xs).

%%
%% cell behaviour
%%

cell() ->
    receive
	{neighbours, Xs} ->
	    continue
    end,
    receive
	{go, Pid, N, Num} ->
	    continue
    end,
    cell_iter(Xs, N, Num, Pid).


cell_iter(_Xs, N, 0, Pid) ->
    Pid ! {self(), N};
cell_iter(Xs, N, Num, Pid) ->
    send_nghs(Xs, N),
    cell_iter(Xs, repro(N, receive_nghs(Xs, 0)), Num-1, Pid).

send_nghs([],_N) ->
    true;
send_nghs([X|Xs], N) ->
    X ! {self(), state, N},
    send_nghs(Xs, N). 

receive_nghs([], N) -> 
    N;
receive_nghs([X|Xs], N) ->
    receive
	{X, state, M} ->
	    receive_nghs(Xs, N+M)
    end.


%%
%% cell reproduction
%%

repro(N, S) ->
    if 
	S <2 -> 0;
        S==2 -> N;
	S==3 -> 1;
	S >3 -> 0
    end.

%%
%% access matrix 
%%

% access(I,J,Matrix) ->
%     nth(J,nth(I,Matrix)).

%%
%% complete torus
%%

complete(Ls) ->
    complete_list(complete_lines(Ls)). 

complete_lines([]) ->
    [];
complete_lines([L|Ls]) ->
    [complete_list(L) | complete_lines(Ls)].

complete_list(Ls) ->
    [last(Ls) | app(Ls, [nth(1,Ls)])].

last([L]) ->
  L;
last([_|M]) -> last(M).

app([H|T],L) -> [H|app(T,L)];
app([],L)    -> L.

nth(1,[E|_]) -> E;
nth(N,[_|L]) -> 
  nth(N-1,L).

