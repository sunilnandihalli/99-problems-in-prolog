add(X,nil,t(X,nil,nil)).
add(X,t(Root,Left,Right),t(Root,LeftNew,Right)):-
	X@<Root,
	add(X,Left,LeftNew).
add(X,t(Root,Left,Right),t(Root,Left,RightNew)):-
	X@>Root,
	add(X,Right,RightNew).

construct(L,T):-construct(L,T,nil).
construct([],T,T).
construct([X|Xs],T,T1):-
	add(X,T1,T2),
	construct(Xs,T,T2).
	
