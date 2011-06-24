combination(0,Rest,[],Rest).
combination(N,L,[X|Xs],Rest):-
	append([Left,[X],Right],L),
	N1 is N-1,
	combination(N1,Right,Xs,Rest1),
	append([Left,Rest1],Rest).
group(_,[],[]).
group(L,[N|Ns],[G|Gs]):-
	combination(N,L,G,Rest),
	group(Rest,Ns,Gs).
	