leaves(nil,[]).
leaves(t(X,L,R),[X|Xs]):-
	leaves(L,Xs1),
	leaves(R,Xs2),
	append([Xs1,Xs2],Xs).