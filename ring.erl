%% file: "ring.erl"

-module(ring).
-export([test/0,compile/1,test/3,foo/0,process/1]).
-compile(export_all).

process(Main) ->
  receive
    Dest -> process(Main,Dest)
  end.

process(Main,Dest) ->
  receive
    terminate -> Dest ! terminate;
    0         -> Dest ! terminate,
                 receive
                   terminate -> Main ! done
                 end;
    X         -> Dest ! (X-1), process(Main,Dest)
  end.

create(_Main,0) -> [];
create(Main,N) -> [spawn(ring,process,[Main])|create(Main,N-1)].

connect(Ps) -> connect(hd(Ps),Ps).
connect(First,[P])        -> P ! First;
connect(First,[P|Others]) -> P ! hd(Others), connect(First,Others).

ring(Nbprocs,Hops) ->
  Ps = create(self(),Nbprocs),
  connect(Ps),
  hd(Ps) ! Hops,
  receive
    done -> ok
  end.

loop(0,R,_Procs,_Msgs) -> R;
loop(N,_R,Procs, Msgs) -> loop(N-1,ring(Procs,Msgs),Procs,Msgs).

test() ->
  time(600,100000).

time(Procs, Msgs) ->
  T1 = bm:time_now(),
  _R = loop(50, 0, Procs, Msgs),
  Time = bm:time_since(T1),
  Time.

compile(Flags) ->
  hipe:c(?MODULE,Flags).

test(Start,Stop,Msg) when Start < Stop ->
  time(Start,Msg),
  test(Start+50,Stop,Msg);
test(_,Stop,Msg) ->
  time(Stop,Msg).

foo() ->
  time(10,1000),
  time(10,10000),
  time(10,100000),
  time(50,100000),
  time(100,100000),
  time(500,100000),
  time(1000,100000),
  time(5000,100000),
  time(10000,100000).

