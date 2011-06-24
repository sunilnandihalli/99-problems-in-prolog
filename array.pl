array_atom(A,[X]):-
	format(atom(A),"~w",X).
array_atom(A,[X|Xs]):-
	array_atom(AXs,Xs),
	format(atom(A),"~w_~w",[X,AXs]).
array([],Name):-
	
	
array([NI|NIs],Name):-
	between(1,NI,I),
	array_atom(IthElement,[Name,I]),
	array(NIs,IthElement).

do(1):-
	array([1,2,3,4],s),write
	