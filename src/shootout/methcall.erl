%%% -*- mode: erlang -*-
%%% $Id: methcall.erlang,v 1.2 2005-05-08 02:02:47 igouy-guest Exp $
%%% http://shootout.alioth.debian.org/
%%%
%%% Provided by Bengt Kleberg (Erlang Guru!)
%%% fixed by Isaac Gouy (Erlang novice)

-module(methcall).  
-export([main/0, main/1]).
-export([small/0,medium/0,big/0]).

small() -> 12.
medium() -> 16.
big() -> 20.

-record( toggle_base, {state, value = fun value/1, activate = fun activate/1} ).
-record( toggle, {toggle_base} ).
-record( nth_toggle, { toggle_base, counter = 0, max_count } ).

main() -> main(["1"]).
main([Arg]) ->
   Number_of_Times = list_to_integer(Arg),
   State = activate_toggle( Number_of_Times, toggle_new( true ) ),
   io:fwrite("~w\n", [State]),
   Nth_State = activate_toggle( Number_of_Times, nth_toggle_new( true, 3 ) ),
   io:fwrite("~w\n", [Nth_State]),
   erlang:exit(ok).

activate_toggle( 0, Toggle ) ->
	Value = (Toggle#toggle.toggle_base)#toggle_base.value,
	Value( Toggle );
activate_toggle( Number_of_Times, Toggle ) ->
	Activate = (Toggle#toggle.toggle_base)#toggle_base.activate,
	activate_toggle( Number_of_Times - 1, Activate( Toggle )).


toggle_base_new( State ) ->
	#toggle_base{state = State}.

toggle_new( State ) ->
	Toggle = toggle_base_new( State ),
	#toggle{toggle_base = Toggle}.

nth_toggle_new( State, Max_Count ) ->
	Toggle = toggle_base_new( State ),
	#nth_toggle{toggle_base = Toggle, max_count = Max_Count - 1}.


activate( #toggle_base{state = true} = Toggle_Base ) ->
	Toggle_Base#toggle_base{state = false};
activate( #toggle_base{state = false} = Toggle_Base ) ->
	Toggle_Base#toggle_base{state = true};
activate( #toggle{toggle_base = Toggle_Base} = Toggle ) ->
	Toggle#toggle{toggle_base = activate( Toggle_Base )};
activate( #nth_toggle{counter = Max_Count, max_count = Max_Count, toggle_base = Toggle_Base} = Nth_Toggle ) ->
	Nth_Toggle#nth_toggle{toggle_base = activate( Toggle_Base ), counter = 0};
activate( #nth_toggle{counter = Count} = Nth_Toggle ) ->
	Nth_Toggle#nth_toggle{counter = Count + 1}.

value( #toggle_base{state = State} ) ->
	State;
value( #toggle{toggle_base = Toggle_Base} ) ->
	value( Toggle_Base );
value( #nth_toggle{toggle_base = Toggle_Base} ) ->
	value( Toggle_Base ).

