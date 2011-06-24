no_collision(Q,Qs):-
	\+ (member(Q1,Qs),Q1=Q),
	\+ (nth1(N,Qs,Q1),N =:= abs(Q1-Q)).

solve(N,Qs):-
	place_queen(N,N,Qs).

place_queen(_,0,[]).
place_queen(N,I,[Q|Qs]):-
	I>0,
	I1 is I-1,
	place_queen(N,I1,Qs),
	between(1,N,Q),
	no_collision(Q,Qs).