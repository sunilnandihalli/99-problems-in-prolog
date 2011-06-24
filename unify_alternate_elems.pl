unify([C,_|Xs],[C,_|Ys]):-
	unify(Xs,Ys).
unify([],[]).
unify([C],[C]).


myl(N,L):-length(L,N).

unify_row([],[],_).
unify_row([X|Xs],[Col|Cols],RowId):-
	nth0(RowId,Col,X),
	unify_row(Xs,Cols,RowId).

unify_rows([],_,_).
unify_rows([Row|Rows],Cols,RowId):-
	unify_row(Row,Cols,RowId),
	RowId1 is RowId+1,
	unify_rows(Rows,Cols,RowId1).


make_rectangle(Rows,Cols,NRows,NCols):-
	length(Rows,NRows),
	length(Cols,NCols),
	maplist(myl(NCols),Rows),
	maplist(myl(NRows),Cols),
	unify_rows(Rows,Cols).
