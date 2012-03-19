%% file: "smith.erl"

-module(smith).
-export([test/0]).

-compile({no_auto_import,[max/2]}).

max(A,B) when is_integer(A), is_integer(B) ->
  if
    A > B -> A;
    true  -> B
  end.

alpha_beta_penalty(A,B) when is_integer(A), is_integer(B) -> max(A-4,B-1).

generate_sequence(Length,R) when is_integer(Length) ->
  if
    Length =:= 0 -> [];
    true         -> [R rem 10 | generate_sequence(Length-1,
                                                  (R * 11 + 1237501)
                                                  rem 10067)]
  end.

generate_sequences(0,_,_) -> [];
generate_sequences(N,Length,R) when is_integer(N), is_integer(Length) ->
  [generate_sequence(Length, R) | generate_sequences(N-1,Length,R+1)].

match_entry(Top,Side,UpperLeft,Upper,Left) when is_integer(Top), is_integer(Side) ->
  MeLeft = alpha_beta_penalty(element(3, Left), element(1, Left)),
  MeUpper = alpha_beta_penalty(element(3, Upper), element(2, Upper)),
  MeUpperLeft =
	if
	    Top =:= Side ->
		max(MeLeft,
		    max(MeUpper,
			max(element(3,UpperLeft)+1,0)));
	    true ->
		max(MeLeft,
		    max(MeUpper,
			max(element(3,UpperLeft),0)))
	end,
  {MeLeft, MeUpper, MeUpperLeft,
   max(MeUpperLeft,
       max(element(4,Left),
           max(element(4,Upper),element(4,UpperLeft))))}.

match_zero_entry(Top,Side,{Left,_,UpperLeft,Max}) when is_integer(Top), is_integer(Side) ->
  ELeft = alpha_beta_penalty(UpperLeft, Left),
  _Weight = max(1-abs(Side-Top),0),
  EUpperLeft = max(max(ELeft,max(1-abs(Side-Top),0)),0),
  EMax = max(max(Max,EUpperLeft),0),
  {ELeft, -1, EUpperLeft, EMax}.

match(Tops,Side,Prev,UpperLeft,Left) ->
  match0(Tops, Side, Prev, UpperLeft, Left, [], none).

match0([Top|Tops],Side,[Upper|Prev],UpperLeft,Left,Acc,_Last) when
  is_integer(Top), is_integer(Side) ->
  E = match_entry(Top, Side, UpperLeft, Upper, Left),
  match0(Tops, Side, Prev, Upper, E, [E|Acc], E);
match0([Top|Tops],Side,none,UpperLeft,Left,Acc,_Last) when
  is_integer(Top), is_integer(Side) ->
  E = match_zero_entry(Top, Side, Left),
  match0(Tops, Side, none, UpperLeft, E, [E|Acc], E);
match0([],_,_,_,_,Acc,Last) -> {Acc,Last}.

match_two_seq(Side,Top,Prev) ->
  match_two_seq0(Side, Top, Prev, none).

match_two_seq0([S|Side],Top,Prev,_Acc) when is_integer(S) ->
  {Row,Result} = match(Top,S,Prev,{0,0,0,0},{0,0,0,0}),
  match_two_seq0(Side, Top, Row, Result);
match_two_seq0([],_,_,Result) -> Result.

match_sequences(Tops,Side) ->
  match_sequences0(Tops, Side, -9999999).

match_sequences0([Top|Tops],Side,CrntResult) ->
  Result = element(4, match_two_seq(Top, Side, none)),
  match_sequences0(Tops, Side, max(CrntResult, Result));
match_sequences0([],_,MaxResult) -> MaxResult.

loop(0,_Tops,_Side,R) -> R;
loop(N,Tops,Side,_R) -> loop(N-1,Tops,Side,match_sequences(Tops,Side)).

test() ->
    Tops = generate_sequences(200,64,1),
    Side = generate_sequence(64,0),
    T1 = run_benchmark:time_now(),
    _R = loop(30,Tops,Side,0),
    Time = run_benchmark:time_since(T1),
    %% io:format("~w\t",[Time]),
    Time.

