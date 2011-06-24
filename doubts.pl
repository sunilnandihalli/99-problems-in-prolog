
predicate(A,B):-!.
predicate(A,B).

investigate(A,B) :-
	between(A,B,N),
	sym_cbal_trees(N,Ts),
	length(Ts,L),
	writef('%w   %w',[N,L]), nl,
    fail.
investigate(_,_).

program 60

program 66

program 80

graph_paint algo....