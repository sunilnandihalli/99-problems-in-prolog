bind_mult_tree(t(a,[t(f,[t(g,[])]),t(c,[]),t(b,[t(d,[]),t(e,[])])])).
bind_mult_tree(t(a,[t(f,[])])).


bind_mult_tree_string('afg^^c^bd^e^^^').
bind_mult_tree_string('af^^').

bind_mult_tree_bottom_up_atom(gfcdeba).
bind_mult_tree_bottom_up_atom(fa).

is_mult_tree(t(_,List)):-
	is_mult_tree_list(List).

is_mult_tree_list([]).
is_mult_tree_list([X|Xs]):-
	is_mult_tree(X),
	is_mult_tree_list(Xs).

nnodes_multitree_list([],0).
nnodes_multitree_list([X|Xs],N):-
	nnodes_multitree(X,N1),
	nnodes_multitree_list(Xs,N2),
	N is N1+N2.

nnodes_multitree(t(_,L),N):-
	nnodes_multitree_list(L,N1),
	N is N1+1.


carat([^|Xs]-Xs).
symbol(X,[X|Xs]-Xs).
letter(X,[X|Xs]-Xs):-char_type(X,alpha).

multi_tree_forest_string_dl([],['^'|L]-L).
multi_tree_forest_string_dl([T|Ts],L1-L3):-
	multi_tree_string_dl(T,L1-L2),
	multi_tree_forest_string_dl(Ts,L2-L3).

multi_tree_string_dl(t(X,F),L1-L3):-
	letter(X,L1-L2),
	multi_tree_forest_string_dl(F,L2-L3).

multi_tree_string(T,A):-
	var(T),!,
	atom_chars(A,S),
	multi_tree_string_dl(T,S-[]).

multi_tree_string(T,A):-
	var(A),!,
	multi_tree_string_dl(T,S-[]),
	atom_chars(A,S).

ipl_multi_tree(T,L):-ipl_multi_tree(T,0,L).

ipl_multi_tree(t(_,F),D,L):-
	DF is D+1,
	ipl_multi_tree_forest(F,DF,LF),
	L is LF+D.

ipl_multi_tree_forest([],_,0).
ipl_multi_tree_forest([T|Ts],D,LF):-
	ipl_multi_tree(T,D,LT),
	ipl_multi_tree_forest(Ts,D,LTs),
	LF is LT+LTs.

bottom_up(T,A):-
	var(A),!,
	bottom_up_f(T,S-[]),
	atom_chars(A,S).

bottom_up(T,A):-
	var(T),!,
	atom_chars(A,S),
	bottom_up_f(T,S-[]).

bottom_up_f(t(X,F),L1-L3):-
	bottom_up_forest_f(F,L1-L2),
	letter(X,L2-L3).

bottom_up_forest_f([],L-L).
bottom_up_forest_f([T|Ts],L1-L3):-
	bottom_up_f(T,L1-L2),
	bottom_up_forest_f(Ts,L2-L3).


tree_ltl(T,L):-tree_ltl_d(T,L-[]).

tree_ltl_d(t(X,[]),[X|L]-L):-!.
tree_ltl_d(t(X,F),L1-L5):-
	symbol('(',L1-L2),
	letter(X,L2-L3),
	tree_ltl_forest_d(F,L3-L4),
	symbol(')',L4-L5).

tree_ltl_forest_d([],L-L).
tree_ltl_forest_d([T|Ts],L1-L3):-
	tree_ltl_d(T,L1-L2),
	tree_ltl_forest_d(Ts,L2-L3).