split(0,0,0):-!.
split(N,N1,N2):-
	N1t is N//2,N2t is N-N1t,
	maxFirst(N1t,N2t,N1,N2).

maxFirst(N1,N2,M1,M2):-
	N1<N2,
	M1 is N2,M2 is N1,!.
maxFirst(N1,N2,N1,N2).


complete_binary_tree(0,nil):-!.
complete_binary_tree(N,t(_,L,R)):-
	N1 is N-1,
	split(N1,M1,M2),
	complete_binary_tree(M1,L),
	complete_binary_tree(M2,R).