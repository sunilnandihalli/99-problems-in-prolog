%bind([fr(a,1),fr(b,2),fr(c,3),fr(d,4)]).
bind([fr(a,45),fr(b,13),fr(c,12),fr(d,16),fr(e,9),fr(f,5)]).

init([],[]).
init([fr(S,F)|Fs],[n(F,S)|Ns]) :- init(Fs,Ns).

find_huffman_code(F,H):-
	init(F,Nus),
	sort(Nus,Ns),
	make_tree(Ns,Tree),
	convert_tree_to_code(Tree,'',H).

convert_tree_to_code(n(_,p(N1,N2)),ICode,Code):-
	atom_concat(ICode,0,ICodeLeft),
	convert_tree_to_code(N1,ICodeLeft,Code1),
	atom_concat(ICode,1,ICodeRight),
	convert_tree_to_code(N2,ICodeRight,Code2),
	append([Code1,Code2],Code).

convert_tree_to_code(n(_,S),ICode,[hc(S,ICode)]):-!.
	
	
make_tree(Ns,Tree):-
	length(Ns,L),
	L1 is L-1,
	merge_the_lowest(Ns,Tree,L1).

merge_the_lowest([X],X,0):-!.
merge_the_lowest(InputTree,OutputTree,L):-
	merge_the_two_lowest(InputTree,Tree1),
	update_the_position_of_the_first_element(Tree1,Tree2),
	L1 is L-1,
	merge_the_lowest(Tree2,OutputTree,L1).


update_the_position_of_the_first_element([n(F1,S1),n(F2,S2)|Xs],
					 [n(F1,S1),n(F2,S2)|Xs]):-F1<F2.
update_the_position_of_the_first_element([n(F1,S1),n(F2,S2)|Xs],
					 [n(F2,S2)|Ys]):-F1>F2,
	update_the_position_of_the_first_element([n(F1,S1)|Xs],Ys).
update_the_position_of_the_first_element([X],[X]):-write(here1),nl,!.


merge_the_two_lowest([n(F1,S1),n(F2,S2)|Ns],[n(F,p(n(F1,S1),n(F2,S2)))|Ns]):-F is F1+F2.
merge_the_two_lowest(_):-!.