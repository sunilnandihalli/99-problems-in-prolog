permutation(0,_,[]).
permutation(K,L,[X|Xs]):-
	K>0,
	select(X,L,Rest),
	K1 is K-1,
	permutation(K1,Rest,Xs).

combination(0,_,[]).
combination(K,L,[X|Xs]):-
	K>0,
	append([_,[X],R],L),
	K1 is K-1,
	combination(K1,R,Xs).

