%%%-------------------------------------------------------------------
%%% File    : yaws_html.erl
%%% Author  : Luke Gorrie <luke@bluetail.com>
%%% Purpose : Yaws HTML benchmark.
%%%
%%% Created : 20 Nov 2003 by Luke Gorrie <luke@bluetail.com>
%%%-------------------------------------------------------------------
-module(yaws_html).
-export([test/0, compile/1]).

test() ->
    T1 = run_benchmark:time_now(),
    ok = ehtml_benchmark(1000000),
    Time = run_benchmark:time_since(T1),
    Time.

%%-export([normal/1,expander/1]).
%%ehtml_benchmark(N) ->
%%  {Normal,ok} = timer:tc(?MODULE, normal, [N]),
%%  {Expand,ok} = timer:tc(?MODULE, expander, [N]),
%%  io:format("Normal: ~pms  Expand: ~pms~n",
%%	      [round(Normal/1000), round(Expand/1000)]).

ehtml_benchmark(N) ->
    ok = normal(N),
    ok = expander(N).

normal(0) ->
    ok;
normal(N) ->
    Expr = {td, [],
	    {table, [{border, "0"}],
	     [
	      {tr, [{valign, bot}], 
	       [ 
		 {td, [{align, center}], 
		  {a, [{href, "$href"}],
		   {img, [{src, "$imgsrc"},
			  {onerror, "image_load_failure(this)"},
			  {border, "0"},
			  {alt, ""},
			  {title,"$title"}]}}},
		 {td, [{align, center}],
		  "$checkbox"}
		]
	      },
	      {tr, [{valign, top}],
	       {td, [{colspan, "2"},
		     {align, center}, {width, "60"}], 
		{a, [{href, "$href"}],
		 {p, [{class, fname}], "$name"}}}}
	     ]
	    }
	   },
    ehtml_expand(Expr),
    normal(N-1).

expander(N) ->
    Expr = {td, [],
	    {table, [{border, "0"}],
	     [
	      {tr, [{valign, bot}], 
	       [ 
		 {td, [{align, center}], 
		  {a, [{href, '$href'}],
		   {img, [{src, '$imgsrc'},
			  {onerror, 'image_load_failure(this)'},
			  {border, "0"},
			  {alt, ""} | '$title']}}},
		 {td, [{align, center}],
		  '$checkbox'}
		]
	      },
	      {tr, [{valign, top}],
	       {td, [{colspan, "2"},
		     {align, center}, {width, "60"}], 
		{a, [{href, '$href'}],
		 {p, [{class, fname}], '$name'}}}}
	     ]
	    }
	   },
    Expand = ehtml_expander(Expr),
    expander(Expand, N).

expander(_Expand, 0) ->
    ok;
expander(Expand, N) ->
    ehtml_apply(Expand,
		[{href,"$href"},
		 {imgsrc,"$imgsrc"},
		 {title,[{title,"$title"}]},
		 {checkbox,"$checkbox"},
		 {name,"$name"}]),
    expander(Expand, N-1).

compile(Flags) ->
    hipe:c(?MODULE,Flags).

%% ------------------------------------------------------------
%% The code below is taken from yaws_api.erl in yaws.


%% ------------------------------------------------------------
%% simple erlang term representation of HTML:
%% EHTML = [EHTML] | {Tag, Attrs, Body} | {Tag, Attrs} | {Tag} |
%%         binary() | character()
%% Tag 	 = atom()
%% Attrs = [{Key, Value}]  or {EventTag, {jscall, FunName, [Args]}}
%% Key 	 = atom()
%% Value = string()
%% Body  = EHTML
ehtml_expand(Ch) when Ch >= 0, Ch =< 255 -> htmlize_char(Ch);
ehtml_expand(Bin) when is_binary(Bin) -> htmlize(Bin);
ehtml_expand({Tag}) -> ehtml_expand({Tag, []});
ehtml_expand({pre_html, X}) -> X;
ehtml_expand({img, Attrs}) -> ["<img", ehtml_attrs(Attrs), ">"];
ehtml_expand({br, Attrs}) -> ["<br", ehtml_attrs(Attrs), ">"];
ehtml_expand({Tag, Attrs}) ->
    ["\n<", atom_to_list(Tag), ehtml_attrs(Attrs), ">"];
ehtml_expand({Tag, Attrs, Body}) when is_atom(Tag) ->
    Ts = atom_to_list(Tag),
    ["\n<", Ts, ehtml_attrs(Attrs), ">", ehtml_expand(Body), "</", Ts, ">"];
ehtml_expand([H|T]) -> [ehtml_expand(H)|ehtml_expand(T)];
ehtml_expand([]) -> [].

ehtml_attrs([]) -> [];
ehtml_attrs([Attribute|Tail]) when is_atom(Attribute) ->
    [[$ |atom_to_list(Attribute)]|ehtml_attrs(Tail)];
ehtml_attrs([{Name, Value} | Tail]) ->
    ValueString = if is_atom(Value) -> [$",atom_to_list(Value),$"];
		     is_list(Value) -> [$",Value,$"];
		     is_integer(Value) -> [$",integer_to_list(Value),$"];
		     is_float(Value) -> [$",float_to_list(Value),$"]
		  end,
    [[$ |atom_to_list(Name)], [$=|ValueString]|ehtml_attrs(Tail)].

%% ------------------------------------------------------------
%% ehtml_expander/1: an EHTML optimizer
%%
%% This is an optimization for generating the same EHTML multiple times with
%% only small differences, by using fast re-usable templates that contain
%% variables. The variables are atoms starting with a dollar sign, like
%% '$myvar'. There are two functions: ehtml_expander/1 to create an optimized
%% EHTML template, then ehtml_apply/2 takes a template and a dictionary of
%% variable values and generates the actual HTML.
%%
%% If you are spending a lot of time regenerating similar EHTML fragments then
%% this is for you.
%%
%% Variables can appear in three places:
%% - As a body element, where you would normally have a tag. The values of
%%   these variables are expanded as EHTML.
%% - As the name or value of an attribute. The values of these variables are
%%   strings.
%% - As the CDR of an attribute list. The values of these variables are
%%   key-value lists of more attributes.
%%
%% The approach is inspired by the way that Yaws already treats .yaws files,
%% and the article ``A Hacker's Introduction To Partial Evaluation'' by Darius
%% Bacon (cool guy), http://www.lisp-p.org/htdocs/peval/peval.cgi
%%
%% (For now I flatter myself that this is some kind of partial evaluator, but
%% I don't really know :-) -luke)

ehtml_expander(X) ->
    ehtml_expander_compress(flatten(ehtml_expander(X, [], [])), []).

%% Returns a deep list of text and variable references (atoms)

%% Text
ehtml_expander(Ch, Before, After) when Ch >= 0, Ch =< 255 ->
    ehtml_expander_done(htmlize_char(Ch), Before, After);
ehtml_expander(Bin, Before, After) when is_binary(Bin) ->
    ehtml_expander_done(htmlize(Bin), Before, After);
ehtml_expander({pre_html, X}, Before, After) ->
    ehtml_expander_done(X, Before, After);
%% Tags
ehtml_expander({Tag}, Before, After) ->
    ehtml_expander({Tag, []}, Before, After);
ehtml_expander({img, Attrs}, Before, After) ->
    ehtml_expander_done(["<img",
			 ehtml_attrs_expander(Attrs),">"],
			Before,
			After);
ehtml_expander({br, Attrs}, Before, After) ->
    ehtml_expander_done(["<br",
			 ehtml_attrs_expander(Attrs),">"],
			Before,
			After);
ehtml_expander({Tag, Attrs}, Before, After) ->
    ehtml_expander_done(["\n<", atom_to_list(Tag),
			 ehtml_attrs_expander(Attrs), ">"],
			Before,
			After);
ehtml_expander({Tag, Attrs, Body}, Before, After) ->
    ehtml_expander(Body,
		   [["\n<", atom_to_list(Tag), ehtml_attrs_expander(Attrs), ">"]|
		    Before],
		   ["</", atom_to_list(Tag), ">"|After]);
%% Variable references
ehtml_expander(Var, Before, After) when is_atom(Var) ->
    [lists:reverse(Before), {ehtml, ehtml_var_name(Var)}, After];
%% Lists
ehtml_expander([H|T], Before, After) ->
    ehtml_expander(T, [ehtml_expander(H, [], [])|Before], After);
ehtml_expander([], Before, After) ->
    ehtml_expander_done("", Before, After).

%% Expander for attributes. The attribute name and value can each be a
%% variable reference.
ehtml_attrs_expander([]) -> "";
ehtml_attrs_expander([{Var,Val}|T]) ->
    [[" ",
      ehtml_attr_part_expander(Var),
      "=",
      "\"", ehtml_attr_part_expander(Val), "\""]|
     ehtml_attrs_expander(T)];
ehtml_attrs_expander(Var) when is_atom(Var) ->
    %% Var in the cdr of an attribute list
    [{ehtml_attrs, ehtml_var_name(Var)}].

ehtml_attr_part_expander(A) when is_atom(A) ->
    case ehtml_var_p(A) of
	true  -> {preformatted, ehtml_var_name(A)};
	false -> atom_to_list(A)
    end;
ehtml_attr_part_expander(I) when is_integer(I) -> integer_to_list(I);
ehtml_attr_part_expander(S) when is_list(S) -> S.

ehtml_expander_done(X, Before, After) -> [lists:reverse([X|Before]), After].

%% Compress an EHTML expander, converting all adjacent bits of text into
%% binaries.
%% Returns: [binary() | {ehtml, Var} | {preformatted, Var}, {ehtml_attrs, Var}]
%% Var = atom()
ehtml_expander_compress([Tag|T], Acc) when is_tuple(Tag) ->
    [list_to_binary(lists:reverse(Acc)), Tag | ehtml_expander_compress(T, [])];
ehtml_expander_compress([], Acc) -> [list_to_binary(lists:reverse(Acc))];
ehtml_expander_compress([H|T], Acc) when is_integer(H) ->
    ehtml_expander_compress(T, [H|Acc]).

%% Apply an expander with the variable bindings in Env.  Env is a list of
%% {VarName, Value} tuples, where VarName is an atom and Value is an ehtml
%% term.
ehtml_apply(Expander, Env) -> [ehtml_eval(X, Env) || X <- Expander].

ehtml_eval(Bin, _Env) when is_binary(Bin) -> Bin;
ehtml_eval({Type, Var}, Env) ->
    case lists:keysearch(Var, 1, Env) of
	false -> exit({ehtml_unbound, Var});
	{value, {Var, Val}} ->
	    case Type of
		ehtml -> ehtml_expand(Val);
		preformatted -> Val;
		ehtml_attrs -> ehtml_attrs(Val)
	    end
    end.

%% Get the name part of a variable reference.
%% e.g. ehtml_var_name('$foo') -> foo.
ehtml_var_name(A) when is_atom(A) ->
    case ehtml_var_p(A) of
	true -> list_to_atom(tl(atom_to_list(A)));
	false -> exit({bad_ehtml_var_name, A})
    end.

%% Is X a variable reference? Variable references are atoms starting with $.
ehtml_var_p(X) when is_atom(X) -> hd(atom_to_list(X)) == $$;
ehtml_var_p(_) -> false.


%% htmlize  

htmlize(<<Char, Tail/binary>>) ->
    case htmlize_char(Char) of
	Char ->
	    <<Char, (htmlize(Tail))/binary>>;
        Bin ->		
            <<Bin/binary, (htmlize(Tail))/binary>>
    end;
htmlize(<<>>) ->			 
    <<>>;
htmlize(List) when is_list(List) ->
    htmlize_l(List).

htmlize_char($>) ->
    <<"&gt;">>;
htmlize_char($<) ->
    <<"&lt;">>;
htmlize_char($&) ->
    <<"&amp;">>;
htmlize_char(X) ->
    X.


%% htmlize list (usually much more efficient than above)
htmlize_l(List) ->
    htmlize_l(List, []).

htmlize_l([], Acc) -> lists:reverse(Acc);
htmlize_l([$>|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$g,$&|Acc]);
htmlize_l([$<|Tail], Acc) ->
    htmlize_l(Tail, [$;,$t,$l,$&|Acc]);
htmlize_l([$&|Tail], Acc) ->
    htmlize_l(Tail, [$;,$p,$m,$a,$&|Acc]);
htmlize_l([X|Tail], Acc) when is_integer(X) ->
    htmlize_l(Tail, [X|Acc]);
htmlize_l([X|Tail], Acc) when is_binary(X) ->
    X2 = htmlize_l(binary_to_list(X)),
    htmlize_l(Tail, [X2|Acc]);
htmlize_l([X|Tail], Ack) when is_list(X) ->
    X2 = htmlize_l(X),
    htmlize_l(Tail, [X2|Ack]).


%% Utilities -- Taken from elsewhere

flatten(List) when is_list(List) ->
    do_flatten(List, []).

do_flatten([H|T], Tail) when is_list(H) ->
    do_flatten(H, do_flatten(T, Tail));
do_flatten([H|T], Tail) ->
    [H|do_flatten(T, Tail)];
do_flatten([], Tail) ->
    Tail.
