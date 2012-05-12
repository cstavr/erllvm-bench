%% File    : w_estone.erl
%% Author  : RHS Linux User <klacke@erix.ericsson.se>
%% Purpose : Measure Erlang implementations on a certain machine
%% Created : 29 Oct 1996 by RHS Linux User <klacke@jb.du.etx.ericsson.se>
%% Modified: 13 Apr 2005 by another RHS Linux User <kostis@it.uu.se>
%%		- changed the export_all to selextive export of functions
%%		- removed unused functions and most Dialyzer warnings

-module(w_estone).
-author('klacke@erix.ericsson.se').

-export([main/1, compile/1, macro/1, micros/0, run_micro/2,
	 int_arith/1, lists/1, msgp/1, msgp_medium/1, msgp_huge/1, p1/1,
	 pattern/1, trav/1, large_dataset_work/1, mk_big_procs/1, big_proc/0,
	 large_local_dataset_work/1, very_big/1, alloc/1, bif_dispatch/1,
	 binary_h/1, echo/1, ets/1, generic/1, gserv/4, handle_call/3, req/2,
	 float_arith/1, fcalls/1, remote0/1, remote1/1, app0/1, app1/1,
	 timer/1, links/1, lproc/1]).
%% The following ones are currently not used in computing the estone
%% ranking, but exported nevertheless...
-export([determine_loop_data/0, port_io/1, ppp/2]).

-define(TOTAL, (3000 * 1000 * 100)).   %% 300 secs
-define(BIGPROCS, 2).
-define(BIGPROC_SIZE, 50).

-define(STONEFACTOR, 31000). %% Factor to make the reference
                             %% implementation to make 1000 ESTONES.


%% This is *the* estone benchmark. It tries to meassure the
%% performance of an Erlang system in a way which makes it possible to
%% compare different implementations of Erlang as well as different HW.
%%
%% estone consists of a number of micro benchmarks that that can be
%% run independently. If all the microbenchmarks are combined, we get
%% a system where Erlang 4.3.1 runs with 1000 estones on an 85 Mhz
%% sparc 5.
%%
%% We always measure wallclock here, which makes it important
%% that the computer which runs the main is not used for anything
%% else but the bench.

%% Each micro bench mark is described by an instance of

-record(micro,
	{function, %% The name of the function implementing the micro
	 weight,   %% How important is this in typical applications ??
	 loops = 100,%% initial data
	 tt1,      %% time to do one round
	 str}).    %% Header string


main([]) ->
    R = tty(all),
    R.

compile(Flags) ->
    hipe:c(?MODULE, Flags).

%% run(Compiler, Version, Flags, Os, CPu, MHz) ->
%%     L = ?MODULE:macro(?MODULE:micros()),
%%     run(Compiler, Version, Flags, Os, CPu, MHz, L).
%%
%% run(Compiler, Version, Flags, Os, CPu, MHz, L) ->
%%     Fname = mk_fname([Compiler, Version, Flags, Os, CPu, MHz]),
%%     {ok, F} = file:open(lists:append(Fname , ".EST"), read_write),
%%     _L2 = [[{compiler, Compiler},
%% 	    {version, Version},
%% 	    {flags, Flags},
%% 	    {os, Os},
%% 	    {cpu, CPu},
%% 	    {mhz, MHz}] | L],
%%     %% io:format(F, "~w.~n", [_L2]),
%%     %% io:format("Output written to ~s~n", [lists:append(Fname , ".EST")]),
%%     file:close(F).

tty(all) ->
    pp(macro(micros())).

find_micro(N, [M|_]) when M#micro.function =:= N ->
    {ok, M};
find_micro(N, [_|T]) ->
    find_micro(N, T);
find_micro(_, []) ->
    notfound.

%% iterate(Ofile, Times) ->
%%     {ok, Out} = file:open(Ofile, write),
%%     iterate2(Out, Times),
%%     file:close(Out).
%%
%% iterate2(_, 0) ->
%%     ok;
%% iterate2(Out, I) ->
%%     pp(Out, macro(micros())),
%%     iterate2(Out, I-1).

%% Return a list of micro's
micros() ->
    [
     #micro{function = int_arith,
	    weight = 3,
	    loops = 4157,
	    str = "Small Integer arithmetics"},

     #micro{function = lists,
	    weight = 10, 
	    loops = 9156,
	    str = "list manipulation"},

     #micro{function = msgp,
	    weight = 10,
	    loops = 1433,
	    str = "small messages"},

     #micro{function = msgp_medium,
	    weight = 14,
	    loops = 1527,
	    str = "medium messages"},
     #micro{function = msgp_huge,
	    weight = 4,
	    loops = 52,
	    str = "huge messages"},

     #micro{function = pattern,
	    weight = 5,
	    loops = 1074,
	    str = "pattern matching"},

     #micro{function = trav,
	    weight = 3,
	    loops = 2247,
	    str = "traverse"},

%%   #micro{function = port_io,
%%	    weight = 12,
%%	    loops = 6070,
%%	    str = "Port i/o"},

     #micro{function = large_dataset_work,
	    weight = 3,
	    loops = 1193,
	    str = "Work with large dataset"},

     #micro{function = large_local_dataset_work,
	    weight = 3,
	    loops = 1174,
	    str = "Work with large local dataset"},

     #micro{function = alloc,
	    weight = 2,
	    loops = 3710,
	    str = "Alloc and dealloc"},

     #micro{function = bif_dispatch,
	    weight = 5,
	    loops = 1623,
	    str = "Bif dispatch"},

     #micro{function = binary_h,
	    weight = 4,
	    loops = 581,
	    str = "Binary handling"},
     #micro{function = ets,
	    weight = 5,
	    loops = 342,
	    str = "ets datadictionary"},
     #micro{function = generic,
	    weight = 8,
	    loops = 7977,
	    str = "Generic server (with timeout)"},
     #micro{function = float_arith,
	    weight = 1,
	    loops = 5526,
	    str = "Float arithmetics"},
     #micro{function = fcalls,
	    weight = 5,
	    loops = 882,
	    str = "Function calls"},

     #micro{function = timer,
	    weight = 2,
	    loops = 2312,
	    str = "Timers"},

     #micro{function = links,
	    weight = 1,
	    loops = 30,
	    str = "Links"}

    ].

macro(Ms) ->
    run_micros(Ms).

run_micros([]) -> 
    %% io:nl(),
    [];
run_micros([H|T]) ->
    R = run_micro(H),
    [R| run_micros(T)].

run_micro(M) ->
    Pid = spawn(?MODULE, run_micro, [self(),M]),
    Res = receive {Pid, Reply} -> Reply end,
    %% timer:sleep(2000), %% give system some time 
    Res.


run_micro(Top, M) ->
    Top ! {self(),  apply_micro(M)}.

apply_micro(M) ->
    {GC0, Words0, _} = statistics(garbage_collection),
    statistics(reductions),
    Before = erlang:now(),
    Compensate = apply_micro(M#micro.function, M#micro.loops),
    After = erlang:now(),
    {GC1, Words1, _} = statistics(garbage_collection),
    {_, Reds} = statistics(reductions),
    Elapsed = subtr(Before, After),
    MilliSecs = (Elapsed - Compensate) div 1000,
    [{title, M#micro.str},
     {tt1, M#micro.tt1},
     {function, M#micro.function},
     {weight_percentage, M#micro.weight},
     {loops, M#micro.loops},
     {millisecs,MilliSecs},
     {estones, (M#micro.weight * M#micro.weight * ?STONEFACTOR) div MilliSecs},
     {gcs, GC1 - GC0},
     {kilo_word_reclaimed, (Words1 - Words0) div 1000},
     {kilo_reductions, Reds div 1000},
     {gc_intensity, gci(Elapsed, GC1 - GC0, Words1 - Words0)}].


pp(Ms) ->
    pp(standard_io, Ms).

pp(_Out, Ms) ->
    {_Total, Stones} = sum_millis(Ms, 0, 0),
    %% io:format(_Out, "**** Total time ~w seconds ****~n", [_Total / 1000]),
    %% io:format(_Out, "**** ESTONES = ~w~n~n", [Stones]),
    %% io:format(_Out, "~-31s      ~-12s  ~-10s %    ~-8s ~n~n",
    %%                 ["    Title", "Time", "Estone", "Loops"]),
    Stones.

sum_millis([], Tot, Stones) -> {Tot, Stones};
sum_millis([H|T], Tot, Sto) -> 
    sum_millis(T, ks(millisecs, H) + Tot, ks(estones, H) + Sto).

%% pp2(_,[]) ->   ok;
%% pp2(Out, [_R|Tail]) ->
%%     io:format(Out, "~-35s  ~-12w  ~-10w ~-2w   ~-8w~n",
%% 	      [ks(title,_R), 
%% 	       ks(millisecs,_R), 
%%                ks(estones,_R),
%%                ks(weight_percentage,_R),
%%                ks(loops,_R)]),
%%     pp2(Out, Tail).

ks(K, L) ->
    {value, {_, V}} = lists:keysearch(K, 1, L),
    V.

subtr(Before, After) ->
    (element(1,After)*1000000000000
     +element(2,After)*1000000+element(3,After)) -
        (element(1,Before)*1000000000000
         +element(2,Before)*1000000+element(3,Before)).

gci(Micros, Words, Gcs) ->
    ((256 * Gcs) / Micros) + (Words / Micros).

apply_micro(Name, Loops) ->
    %% io:format("~w(~w) ", [Name, Loops]),
    ?MODULE:Name(Loops).

%%%%%%%%%%%% micro bench manipulating lists. %%%%%%%%%%%%%%%%%%%%%%%%%
lists(0) ->
    0;
lists(I) ->
    L1 = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx",
    L2 = "aaaaaaaaaa",
    revt(10, L1),
    appt(10, L1, L2),
    lists(I-1).

revt(0, _) -> 
    done;
revt(I, L) -> 
    reverse(L),
    revt(I-1, L).

reverse(L) ->
    reverse(L, []).

reverse([H|T], Ack) -> reverse(T, [H|Ack]);
reverse([], Ack) -> Ack.

append([H|T], L) ->
    [H | append(T, L)];
append([], L) ->
    L.

appt(0,_L1,_L2) -> ok;
appt(I, L1, L2) ->
    append(L1, L2),
    appt(I-1, L1, L2).


%%%%%%%%%%%%%%% small message passing and ctxt switching %%%%%%%
msgp(I) ->
    msgp(I, small()).

msgp(0, _) -> 
    0;
msgp(I, Msg) ->
    P1 = spawn(?MODULE, p1, [self()]),
    P2 = spawn(?MODULE, p1, [P1]),
    P3 = spawn(?MODULE, p1, [P2]),
    P4 = spawn(?MODULE, p1, [P3]),
    msgp_loop(100, P4, Msg),
    msgp(I-1, Msg).

p1(To) ->
    receive
	{_From, {message, X}} ->
	    To ! {self(), {message, X}},
	    p1(To);
	stop ->
	    To ! stop,
	    exit(normal)
    end.

msgp_loop(0, P, _) ->
    P ! stop,
    receive 
	stop -> ok
    end;
msgp_loop(I, P, Msg) ->
    P ! {self(), {message, Msg}},
    receive
	{_From, {message, _}} ->
	    msgp_loop(I-1, P, Msg)
    end.

%%%%%%%%%%%% large message passing and ctxt switching %%%%%%%
msgp_medium(I) ->
        msgp_medium(I, big()).

msgp_medium(0, _) -> 
    0;
msgp_medium(I, Msg) ->
    P1 = spawn(?MODULE, p1, [self()]),
    P2 = spawn(?MODULE, p1, [P1]),
    P3 = spawn(?MODULE, p1, [P2]),
    P4 = spawn(?MODULE, p1, [P3]),
    msgp_loop(100, P4, Msg),
    msgp_medium(I-1, Msg).



%%%%%%%%%%%% huge message passing and ctxt switching %%%%%%%
msgp_huge(I) ->
        msgp_huge(I, very_big(15)).

msgp_huge(0, _) -> 
    0;
msgp_huge(I, Msg) ->
    P1 = spawn(?MODULE, p1, [self()]),
    P4 = spawn(?MODULE, p1, [P1]),
    msgp_loop(100, P4, Msg),
    msgp_huge(I-1, Msg).

    
%%%%%% typical protocol pattern matching %%%%%%%
pattern(0) ->
    0;
pattern(I) ->
    Tail = "aaabbaaababba",
    P1 = [0,1,2,3,4,5|Tail],
    pat_loop1(100, P1),
    pat_loop2(100, P1),
    pat_loop3(100, P1),
    pat_loop4(100, P1),
    pat_loop5(100, P1),
    pattern(I-1).

pat_loop1(0, _) -> 
    ok;
pat_loop1(_, [_,_X,_Y, 0 | _])  ->
    ok;
pat_loop1(_, [_,_X,_Y, 1 | _]) ->
    ok;
pat_loop1(_, [_,_X,_Y, 2 | _]) -> 
    ok;
pat_loop1(I, [_, X, Y, 3 | T]) ->
    pat_loop1(I-1, [0, X, Y, 3 | T]).

pat_loop2(0, _) ->
    ok;
pat_loop2(_, [_, Y |_Tail]) when Y bsl 1 =:= 0 ->
    ok;
pat_loop2(_, [_, Y |_Tail]) when Y bsl 2 =:= 0 ->
    ok;
pat_loop2(I, [X, Y | Tail]) when Y bsl 2 =:= 4 ->
    pat_loop2(I-1, [X, Y |Tail]).

pat_loop3(0, _) ->
    ok;
pat_loop3(_, [{c, h} | _]) -> 
    ok;
pat_loop3(_, [1, 0 | _]) ->
    ok;
pat_loop3(_, [X, _ | _]) when byte_size(X) =:= 1 ->
    ok;
pat_loop3(_, [no,_ | _]) -> 
    ok;
pat_loop3(_, []) ->
    ok;
pat_loop3(_, [X, _ | _]) when X =/= 0 ->
    ok;
pat_loop3(_, [2, 3 | _]) ->
    ok;
pat_loop3(_, [1, 2]) ->
    ok;
pat_loop3(I, [0, 1 | T]) ->
    pat_loop3(I-1, [0, 1 | T]).


pat_loop4(0, _) ->  ok;
pat_loop4(_, [20|_]) -> ok;
pat_loop4(_, [219|_]) -> ok;
pat_loop4(_, [18|_]) -> ok;
pat_loop4(_, [17|_]) -> ok;
pat_loop4(_, [16|_]) -> ok;
pat_loop4(_, [15|_]) -> ok;
pat_loop4(_, [14|_]) -> ok;
pat_loop4(_, [13|_]) -> ok;
pat_loop4(_, [12|_]) -> ok;
pat_loop4(_, [11|_]) -> ok;
pat_loop4(_, [10|_]) -> ok;
pat_loop4(_, [9|_]) -> ok;
pat_loop4(_, [8|_]) -> ok;
pat_loop4(_, [7|_]) -> ok;
pat_loop4(_, [6|_]) -> ok;
pat_loop4(_, [5|_]) -> ok;
pat_loop4(_, [4|_]) -> ok;
pat_loop4(_, [3|_]) -> ok;
pat_loop4(_, [1|_]) -> ok;
pat_loop4(_, [21|_]) -> ok;
pat_loop4(_, [22|_]) -> ok;
pat_loop4(_, [23|_]) -> ok;
pat_loop4(_, [24|_]) -> ok;
pat_loop4(_, [25|_]) -> ok;
pat_loop4(_, [26|_]) -> ok;
pat_loop4(_, [27|_]) -> ok;
pat_loop4(I, [0|T]) -> 
    pat_loop4(I-1, [0|T]).

pat_loop5(0, _) -> ok;
pat_loop5(_, [0, 20|_]) -> ok;
pat_loop5(_, [0, 19|_]) -> ok;
pat_loop5(_, [0, 18|_]) -> ok;
pat_loop5(_, [0, 17|_]) -> ok;
pat_loop5(_, [0, 16|_]) -> ok;
pat_loop5(_, [0, 15|_]) -> ok;
pat_loop5(_, [0, 14|_]) -> ok;
pat_loop5(_, [0, 13|_]) -> ok;
pat_loop5(_, [0, 12|_]) -> ok;
pat_loop5(_, [0, 11|_]) -> ok;
pat_loop5(_, [0, 10|_]) -> ok;
pat_loop5(_, [0, 9|_]) -> ok;
pat_loop5(_, [0, 8|_]) -> ok;
pat_loop5(_, [0, 7|_]) -> ok;
pat_loop5(_, [0, 6|_]) -> ok;
pat_loop5(I, [0, 1|T]) -> 
    pat_loop5(I-1, [0,1|T]).

%%%%%%%%%% term traversal representing simple pattern matchhing %%%
%%%%%%%%%                              + some arith
trav(I) ->
    X = very_big(10),
    trav(I, X).

trav(0, _) -> 0;
trav(I, T) ->
    do_trav(T),
    trav(I-1, T).

do_trav(T) when is_tuple(T) ->
    tup_trav(T, 1, 1 + size(T));
do_trav([H|T]) ->
    do_trav(H) + do_trav(T);
do_trav(X) when is_integer(X) -> 1;
do_trav(_) -> 0.

tup_trav(_, P, P) -> 0;
tup_trav(T, P, End) ->
    do_trav(element(P, T)) + tup_trav(T, P+1, End).


%% Port I/O
port_io(I) ->
    Pps = make_port_pids(5, I),  %% 5 ports
    wait_for_pids(Pps),
    0.

make_port_pids(0, _) -> 
    [];
make_port_pids(I, J) ->
    [spawn(?MODULE, ppp, [self(),J]) | make_port_pids(I-1, J)].

ppp(Top, I) ->
    P = open_port({spawn, cat}, []),%% cat sits at the other end
    Str = lists:duplicate(200, 88), %% 200 X'es
    Cmd = {self(), {command, Str}},
    ppp_loop(P, I, Cmd),
    Cmd2 = {self(), {command, "abcde"}},
    Res = ppp_loop(P, I, Cmd2),
    P ! {self(), close},
    receive
	{P, closed} ->
	    closed
    end,
    Top ! {self(), Res}.
    
ppp_loop(_, 0, _) ->
    ok;
ppp_loop(P, I, Cmd) ->
    P ! Cmd,
    receive
	{P, _} ->  %% no match
	    ppp_loop(P, I-1, Cmd)
    end.

%% Working with a very large non-working data set
%% where the passive data resides in remote processes
large_dataset_work(I) ->
    {Minus, Ps} = timer:tc(?MODULE, mk_big_procs, [?BIGPROCS]),
    trav(I),
    lists(I),
    send_procs(Ps, stop),
    Minus. %% Don't count time to create the big procs.

mk_big_procs(0) -> [];
mk_big_procs(I) ->
    [mk_big_proc()| mk_big_procs(I-1)].

mk_big_proc() ->
    P = spawn(?MODULE, big_proc, []),
    P ! {self(), running},
    receive
	{P, yes} -> P
    end.

big_proc() ->
    X = very_big(?BIGPROC_SIZE), %% creates a big heap
    Y = very_big(?BIGPROC_SIZE),
    Z = very_big(?BIGPROC_SIZE),

    receive
	{From, running} ->
	    From ! {self(), yes}
    end,
    receive
	stop ->
	    {X, Y, Z}  %% Can't be garbed away now by very (not super)
                       %% smart compiler
    end.

%% Working with a large non-working data set
%% where the data resides in the local process.
large_local_dataset_work(I) ->
    {Minus,_Data} = timer:tc(?MODULE, very_big, [?BIGPROC_SIZE]),
    trav(I),
    lists(I),
    Minus.


%% Fast allocation and also deallocation that is gc main
%% Important to not let variable linger on the stack un-necessarily
alloc(0) -> 0;
alloc(I) ->
    _X11 = very_big(),
    _X12 = very_big(),
    _X13 = very_big(),
    _Z = [_X14 = very_big(),
	 _X15 = very_big(),
	 _X16 = very_big()],
    _X17 = very_big(),
    _X18 = very_big(),
    _X19 = very_big(),
    _X20 = very_big(),
    _X21 = very_big(),
    _X22 = very_big(),
    _X23 = very_big(),
    _X24 = very_big(),
    alloc(I-1).

%% Time to call bif's
%% Lot's of element stuff which reflects the record code which
%% is becomming more and more common
bif_dispatch(0) ->
    0;
bif_dispatch(I) ->
    disp(),    disp(),    disp(),    disp(),    disp(),    disp(),
    disp(),    disp(),    disp(),    disp(),    disp(),    disp(),
    bif_dispatch(I-1).

disp() ->
    Tup = {a},
    L = [x],
    _ = self(), _ = self(), _ = self(), _ = self(), _ = self(),
    _ = self(), _ = self(), _ = self(), _ = self(),
    _ = make_ref(),
    _ = atom_to_list(''),
    _ = list_to_atom([]),
    _ = tuple_to_list({}),
    _ = list_to_tuple([]),
    _ = element(1, Tup), _ = element(1, Tup), _ = element(1, Tup),
    _ = element(1, Tup), _ = element(1, Tup), _ = element(1, Tup),
    _ = element(1, Tup), _ = element(1, Tup), _ = element(1, Tup),
    _ = element(1, Tup), _ = element(1, Tup), _ = element(1, Tup),
    _ = element(1, Tup), _ = element(1, Tup), _ = element(1, Tup),
    _ = element(1, Tup), _ = element(1, Tup), _ = element(1, Tup),
    _ = setelement(1, Tup, k), _ = setelement(1, Tup, k),
    _ = setelement(1, Tup, k), _ = setelement(1, Tup, k),
    _ = setelement(1, Tup, k), _ = setelement(1, Tup, k),
    _ = setelement(1, Tup, k), _ = setelement(1, Tup, k),
    _ = setelement(1, Tup, k), _ = setelement(1, Tup, k),
    _ = setelement(1, Tup, k), _ = setelement(1, Tup, k),
    _ = setelement(1, Tup, k),
    _ = date(), _ = time(),
    put(a, 1),
    _ = get(a),
    erase(a),
    _ = hd(L),
    _ = tl(L),
    _ = length(L), _ = length(L), _ = length(L), _ = length(L),
    _ = node(), _ = node(), _ = node(), _ = node(),
    _ = node(), _ = node(), _ = node(), _ = node(),
    S = self(),
    node(S), node(S), node(S),
    _ = size(Tup),
    whereis(code_server), whereis(code_server),
    whereis(code_server), whereis(code_server),
    whereis(code_server), whereis(code_server),
    whereis(code_server).
    
    
%% Generic server like behaviour
generic(I) ->
    register(funky, spawn(?MODULE, gserv, [funky, ?MODULE, [], []])),
    g_loop(I).

g_loop(0) ->
    exit(whereis(funky), kill),
    0;
g_loop(I) ->
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [xyz]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [xyz]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    ?MODULE:req(funky, {call, [abc]}),
    g_loop(I-1).

req(Name, Req) ->
    R = make_ref(),
    Name ! {self(), R, Req},
    receive
	{Name, R, Reply} -> Reply
    after 2000 ->
	    exit(timeout)
    end.

gserv(Name, Mod, State, Debug) ->
    receive
	{From, Ref, {call, Req}} when Debug =:= [] ->
	    case catch apply(Mod, handle_call, [From, State, Req]) of
		{reply, Reply, State2} ->
		    From ! {Name, Ref, Reply},
		    gserv(Name, Mod, State2, Debug);
		{noreply, State2} ->
		    gserv(Name, Mod, State2, Debug);
		{'EXIT', Reason} ->
		    exit(Reason)
	    end;
	{_From, _Ref, _Req} when Debug /= [] ->
	    exit(nodebug)
    end.

handle_call(_From, _State, [xyz]) ->
    R = atom_to_list(xyz),
    {reply, R, []};
handle_call(_From, State, [abc]) ->
    R = 1 + 3,
    {reply, R, [R | State]}.

		    

%% Binary handling, creating, manipulating and sending binaries
binary_h(I) ->
    T1 = erlang:now(),
    P = spawn(?MODULE, echo, [self()]),
    B = list_to_binary(lists:duplicate(2000, 5)),
    T2 = erlang:now(),
    Compensate = subtr(T2, T1),
    binary_h_2(I, P, B),
    Compensate.
    
binary_h_2(0, P, _) ->
    exit(P, kill);
binary_h_2(I, P, B) ->
    echo_loop(P, 20, B),
    split_loop(B, {abc,1,2222,self(),"ancnd"}, 100),
    binary_h_2(I-1, P, B).

split_loop(_, _, 0) -> 
    ok;
split_loop(B, Term, I) ->
    {X, Y} = split_binary(B, I),
    _ = size(X),
    _ = binary_to_list(Y, 1, 2),
    binary_to_term(term_to_binary(Term)),
    split_loop(B, Term, I-1).
    

echo_loop(_, 0, _) -> 
    k;
echo_loop(P, I, B) ->
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    P ! B,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    receive _ -> ok end,
    echo_loop(P, I-1, B).
    

ets(0) -> 
    0;
ets(I) ->
    T1 = ets:new(a, [set]),
    T2 = ets:new(c, [bag, private]),
    L = [T1, T2],
    run_tabs(L, L, 1),
    ets:delete(T1),
    ets:delete(T2),
    ets(I-1).

run_tabs(_, _, 0) ->
    ok;
run_tabs([], L, I) ->
    run_tabs(L, L, I-1);
run_tabs([Tab|Tail], L, I) ->
    Begin = I * 20,
    End = (I+1) * 20,
    run_tab(Tab, Begin, End, I),
    run_tabs(Tail, L, I).

run_tab(_Tab, X, X, _) ->
    ok;
run_tab(Tab, Beg, End, J) ->
    ets:insert(Tab, {Beg, J}),
    ets:insert(Tab, {J, Beg}),
    ets:insert(Tab, {{foo,Beg}, J}),
    ets:insert(Tab, {{foo, J}, Beg}),
    ets:delete(Tab, haha),
    ets:match_delete(Tab, {k, j}),
    ets:match(Tab, {Beg, '$1'}),
    ets:match(Tab, {'$1', J}),
    ets:delete(Tab, Beg),
    K = ets:first(Tab),
    _K2 = ets:next(Tab, K),
    run_tab(Tab, Beg+1, End, J).
    
    
%%%% Integer arith %%%%%
int_arith(0) -> 
    0;
int_arith(I) ->
    _ = do_arith(I) +
	do_arith(I) +
	do_arith(I) +
	do_arith(I) +
	do_arith(I) +
	do_arith(I) +
	do_arith(I) +
	do_arith(I) +
	do_arith(I) +
	66,
    int_arith(I-1).

do_arith(I) ->    
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
    do_arith2(I) -
	99.

do_arith2(I) ->
    X = 23,
    _Y = 789 + I,
    Z = I + 1,
    U = (X bsl 1 bsr I) * X div 2 bsr 4,
    U1 = Z + Z + Z + Z + X bsl 4 * 2 bsl 2,
    Z - U + U1 div 2.

    
%%%% Float arith %%%%%
float_arith(0) -> 
    0;
float_arith(I) ->
    _ = f_do_arith(I) +
	f_do_arith(I) +
	f_do_arith(I) +
	f_do_arith(I) +
	f_do_arith(I) +
	f_do_arith(I) +
	f_do_arith(I) +
	f_do_arith(I) +
	f_do_arith(I) +
	66,
    float_arith(I-1).

f_do_arith(I) ->    
    X = 23.4,
    _Y = 789.99 + I,
    Z = I + 1.88,
    U = (X * 1 / I) * X / 2 * 4,
    U1 = Z + Z + Z + Z + X * 4 * 2 / 2,
    Z - U + U1 / 2.

%%%% time to do various function calls
fcalls(0) -> 
    0;
fcalls(I) ->
    local0(400),
    remote0(400),
    app0(400),
    local1(400),
    remote1(400),
    app1(400),
    fcalls(I-1).


local0(0) -> 0;
local0(N) ->
    local0(N-1).

local1(0) -> 0;
local1(N) ->
    1+local1(N-1).

remote0(0) -> 0;
remote0(N) ->
    ?MODULE:remote0(N-1).

remote1(0) -> 0;
remote1(N) ->
    1 + ?MODULE:remote1(N-1).

app0(0) -> 0;
app0(N) ->
    apply(?MODULE, app0, [N-1]).

app1(0) -> 0;
app1(N) ->
    1 + apply(?MODULE, app1, [N-1]).

%%%%%% jog the time queue implementation
timer(I) ->
    L = [50, 50, 50, 100, 1000, 3000, 8000, 50000, 100000],
    timer(I, L).

timer(0, _) -> 0;
timer(N, L) ->
    send_self(100),
    recv(100,L, L),
    timer(N-1).

recv(0, _, _) ->
    ok;
recv(N, [], L) ->
    recv(N, L, L);
recv(N, [Timeout|Tail], L) ->
    receive
        hi_dude ->
            recv(N-1, Tail, L)
    after Timeout ->
            io:format("XXXXX this wasn't supposed to happen???~n", []),
            ok
    end.

send_self(0) ->
    ok;
send_self(N) ->
    self() ! hi_dude,
    send_self(N-1).


%%%%%%%%%%%% managing many links %%%%%
links(I) ->
    L = mk_link_procs(100),
    send_procs(L, {procs, L, I}),
    wait_for_pids(L),
    send_procs(L, die),
    wait_for_pids(L),
    0.

mk_link_procs(0) -> 
    [];
mk_link_procs(I) ->
    [spawn_link(?MODULE, lproc, [self()]) | mk_link_procs(I-1)].


lproc(Top) ->
    receive
	{procs, Procs, I} ->
	    Top ! {self(), lproc(Procs, Procs, link, I)}
    end,
    %% all siblings must have completed their link/unlink
    %% sequences before we are allowed to terminate
    receive
	die -> Top ! {self(), dying}
    end.

lproc(_, _, _, 0) ->
    done;
lproc([], Procs, link, I) ->
    lproc(Procs, Procs, unlink, I-1);
lproc([], Procs, unlink, I) ->
    lproc(Procs, Procs, link, I-1);
lproc([Pid|Tail], Procs, unlink, I) ->
    unlink(Pid),
    lproc(Tail, Procs, unlink, I);
lproc([Pid|Tail], Procs, link, I) ->
    link(Pid),
    lproc(Tail, Procs, unlink, I).



%%%%%%%%%%% various utility functions %%%%%%%

echo(Pid) ->
    receive
	X -> Pid ! X,
	     echo(Pid)
    end.

very_big() -> 
    very_big(2).
very_big(0) -> [];
very_big(I) ->
    {1,2,3,a,v,f,r,t,y,u,self(), self(), self(), 
     "22222222222222222", {{"234", self()}}, 
     [[very_big(I-1)]]}.
 
big() ->
    {self(), funky_stuff, baby, {1, [123, true,[]], "abcdef"}}.

small() -> {self(), true}.    
    
%% Wait for a list of children to respond    
wait_for_pids([]) -> 
    ok;
wait_for_pids([P|Tail]) ->
    receive 
	{P,_Res} -> wait_for_pids(Tail)
    end.

send_procs([P|Tail], Msg) -> P ! Msg, send_procs(Tail, Msg);
send_procs([], _) -> ok.
			     
%% Time to perform one subtraction + one function call
%% loopt() ->
%%    Before = erlang:now(),
%%    loopt(10000),
%%    After = erlang:now(),
%%    subtr(Before, After) / 10000.

%% loopt(0) -> ok;
%% loopt(I) -> loopt(I-1).


%% This function is run be me only (me being klacke) when
%% I've determined how important the different micros are.
%% The purpose is to determine the loops field for each micro
%% in the final bench.

determine_loop_data() ->
    determine_loop_data(1).

determine_loop_data(I) ->
    ppld(determine_loop_data(I, micros())).

determine_loop_data(0, _Ms) ->
    [];
determine_loop_data(I, Ms) ->
    %% io:format("Determining loop counts \n",[]),
    Ms2 = case sum_micros(Ms) of
	      100 ->
		  Tmp = tt1(Ms),
		  set_loops(Tmp);
	      _Other ->
		  %io:format("Micros don't sum up ~w~n", [_Other]),
		  exit(badsum)
	  end,
    [Ms2 | determine_loop_data(I-1, Ms2)]. %% fixpoint iterate
    
ppld(Mss) ->
    ppld(micros(), Mss).
ppld([], _) ->
    ok;
ppld([M| Tail], Mss) ->
    _Str = M#micro.str,
    _Loops = find_loops(M#micro.function, Mss),
    io:format("~-20s  Adjusted Mean: ~-5w   ~w~n", [_Str,mean(_Loops),_Loops]),
    ppld(Tail, Mss).

mean(L0) ->
    L1 = lists:sort(L0),  %% remove the 2 extremes
    L = lists:delete(lists:last(L1), tl(L1)),
    round(sum(L) / length(L)).

sum([]) -> 0;
sum([H|T]) -> H + sum(T).

find_loops(Fn, [MicroList | Tail]) ->
    {ok, M} = find_micro(Fn, MicroList),
    [M#micro.loops | find_loops(Fn, Tail)];
find_loops(_, []) ->
    [].

    

%% We want the macro to take ?TOTAL microsecs
%% This loop runs micro M, M#micro.loops times,
%% and the figures out how what the proper loops value
%% should be in order to spend wight percent of tge ?TOTAL time
%% doing micro M (on the reference implementation).

set_loops([M|Tail]) ->
    TT1 = M#micro.tt1,
    MicroTime = (M#micro.weight * ?TOTAL * 0.01),
    Loops = round(MicroTime / TT1),
    if
	Loops =:= M#micro.loops -> 
	    %io:format("~s is the same~n", [M#micro.str]),
	    ok;
	true ->
	    io:format("Adjusting ~s loop val from ~w to ~w~n",
		      [M#micro.str, M#micro.loops, Loops])
    end,

    [M#micro{loops = Loops} | set_loops(Tail)];
set_loops([]) ->
    [].

sum_micros([]) -> 0;
sum_micros([H|T]) -> H#micro.weight + sum_micros(T).

tt1([M|Tail]) ->
    %% M2 = M#micro{loops = ?TEST_LOOP_COUNT},
    {value, {_, Ms}} = lists:keysearch(millisecs, 1, run_micro(M)),
    [M#micro{tt1 = 1000 * (Ms / M#micro.loops)} | tt1(Tail)];
tt1([]) -> 
    io:nl(),
    [].

%% mk_fname([Hz]) -> integer_to_list(Hz);
%% mk_fname([H|T]) when is_atom(H) -> 
%%    lists:concat([atom_to_list(H) , "-" , mk_fname(T)]);
%% mk_fname([H|T]) -> lists:concat([H, "-", mk_fname(T)]).

