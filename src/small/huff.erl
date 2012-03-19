%%
%% By Christer Jonsson
%%
%% History:
%%          2000-06-14 EJ   File IO Moved outside timing
%%

-module(huff).
-export([test/0,compile/1]).

test() ->
    garbage_collect(),
    statistics(runtime),
    Data = get_data("uudecode.doc"),
    {_,_LoadTime} = statistics(runtime),
    _OrgLength = length(Data),
    T1 = run_benchmark:time_now(),
    R = loop(60,Data,0),
    Time = run_benchmark:time_since(T1),
    _ = length(R),
    Time.

loop(0,_Data,R) -> R;
loop(N,Data,_R) -> loop(N-1,Data,pack_unpack(Data)).

compile(Flags) ->
    hipe:c(?MODULE,Flags).

get_data(FileName) ->
   {ok, _Dev, Fullname} =
     file:path_open(code:get_path(), FileName, [read]),
   {ok, Binary} = file:read_file(Fullname),
   binary_to_list(Binary).

pack_unpack(Data) ->
   OrgSize = length(Data),
   FT = build_freq_trees(Data),
   Trees = sort_trees(FT, []),
   CodeTree = build_code_tree(Trees),
   Codes = make_codes(CodeTree, []),
   Bits = pack_data(Codes, Data),
   Bytes = bits_to_bytes(Bits),
   unpack(OrgSize, CodeTree, bytes_to_bits(Bytes)).


unpack(0,_CodeTree,_Bits) ->
   [];
unpack(Size, CodeTree, Bits) ->
   {Byte, RestBits} = find_byte(CodeTree, Bits),
   [Byte | unpack(Size-1, CodeTree, RestBits)].


find_byte({_, leaf, Byte}, Bits) ->
   {Byte, Bits};
find_byte({_, L,_R}, [1|Bits]) ->
   find_byte(L, Bits);
find_byte({_,_L, R}, [0|Bits]) ->
   find_byte(R, Bits).


bytes_to_bits([]) ->
    [];
bytes_to_bits([Byte|Bytes]) ->
    B7 = Byte div 128,
    B6 = (Byte-128*B7) div 64,
    B5 = (Byte-128*B7-64*B6) div 32,
    B4 = (Byte-128*B7-64*B6-32*B5) div 16,
    B3 = (Byte-128*B7-64*B6-32*B5-16*B4) div 8,
    B2 = (Byte-128*B7-64*B6-32*B5-16*B4-8*B3) div 4,
    B1 = (Byte-128*B7-64*B6-32*B5-16*B4-8*B3-4*B2) div 2,
    B0 = (Byte-128*B7-64*B6-32*B5-16*B4-8*B3-4*B2-2*B1),
    [B7,B6,B5,B4,B3,B2,B1,B0 | bytes_to_bits(Bytes)].


bits_to_bytes([B7, B6, B5, B4, B3, B2, B1, B0 | Rest]) ->
    [(B7*128+B6*64+B5*32+B4*16+B3*8+B2*4+B1*2+B0) | bits_to_bytes(Rest)];
bits_to_bytes([B7, B6, B5, B4, B3, B2, B1]) ->
    [B7*128+B6*64+B5*32+B4*16+B3*8+B2*4+B1*2];
bits_to_bytes([B7, B6, B5, B4, B3, B2]) ->
    [B7*128+B6*64+B5*32+B4*16+B3*8+B2*4];
bits_to_bytes([B7, B6, B5, B4, B3]) ->
    [B7*128+B6*64+B5*32+B4*16+B3*8];
bits_to_bytes([B7, B6, B5, B4]) ->
    [B7*128+B6*64+B5*32+B4*16];
bits_to_bytes([B7, B6, B5]) ->
    [B7*128+B6*64+B5*32];
bits_to_bytes([B7, B6]) ->
    [B7*128+B6*64];
bits_to_bytes([B7]) ->
    [B7*128];
bits_to_bytes([]) ->
    [].


pack_data(_Codes, []) ->
   [];
pack_data(Codes, [Byte|Rest]) ->
   append(get_code(Byte, Codes),pack_data(Codes, Rest)).


get_code(Index, [{I, Bits}|_]) when Index =:= I ->
   Bits;
get_code(Index, [_|Rest]) ->
   get_code(Index, Rest);
get_code(_Index, []) ->
    io:format("error\n",[]),
    exit(error).


make_codes(Tree, Bits) ->
   make_codes(Tree,Bits,[]).

make_codes({_, leaf, Byte}, Bits, Acc) ->
    [{Byte, reverse(Bits)}|Acc];
make_codes({_, R, L}, Bits,Acc) ->
    make_codes(R, [1|Bits], make_codes(L, [0|Bits], Acc)).

%%
%% Make one huffman tree out of a list of trees.
%%

build_code_tree([Tree]) ->
   Tree;
build_code_tree([{Val1, R1, L1}, {Val2, R2, L2} | Rest]) ->
   build_code_tree(insert_tree({Val1+Val2, {Val1, R1, L1}, {Val2, R2, L2}},
			       Rest)).

%%
%% Sort a list of leaves so that those with least frequence is first.
%% Nodes with frequence 0 are removed.
%%

sort_trees([], Sorted) ->
   Sorted;
sort_trees([T|Trees], Sorted) ->
   sort_trees(Trees, insert_tree(T, Sorted)).


%%
%% Insert a tree in a sorted list (least frequencey first)
%%

insert_tree({0, _, _}, []) ->
   [];
insert_tree(Tree, []) ->
   [Tree];
insert_tree({0, _, _}, Trees)  ->
   Trees;
insert_tree({Val1, R1, L1}, [{Val2, R2, L2}|Rest]) when Val1 < Val2 ->
   [{Val1, R1, L1}, {Val2, R2, L2}|Rest];
insert_tree(T1, [T2|Rest]) ->
   [T2|insert_tree(T1, Rest)].


%%
%% Makes a list of 256 leaves each containing the frequency of a bytecode.
%%

build_freq_trees(Data) ->
   build_freq_table(Data, 0).

build_freq_table(_, 256) ->
   [];
build_freq_table(Data, X) ->
   [{occurs(X, Data, 0), leaf, X} | build_freq_table(Data, X+1)].


occurs(_, [], Ack) ->
   Ack;
occurs(X, [Y|Rest], Ack) when X == Y ->
   occurs(X, Rest, Ack+1);
occurs(X, [_|Rest],Ack) ->
   occurs(X, Rest, Ack).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%5
%%
%% Some utilities
%%

reverse(X) ->
   reverse(X, []).
reverse([H|T], Y) ->
   reverse(T, [H|Y]);
reverse([], X) ->
   X.

append([H|T], Z) ->
   [H|append(T, Z)];
append([], X) ->
   X.
