permute([],[],[]).
permute([X1|X1s],L2,[m(X1,X2)|Ms]):-
	select(X2,L2,X2s),
	permute(X1s,X2s,Ms).
