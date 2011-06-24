adxdy(1,2).
adxdy(2,1).
plus_or_minus(X,-X).
plus_or_minus(X,X).
dxdy(X,Y):-
	adxdy(I,J),
	plus_or_minus(I,X),
	plus_or_minus(J,Y).
	
next_move(X,Y):-
	var(X),!,
	next_move(Y,X).

next_move(p(I,J),p(I1,J1)):-
	dxdy(DX,DY),
	I1 is  I+DX,
	between(1,8,I1),
	J1 is J+DY,
	between(1,8,J1).

move(1,[p(1,1)]).
move(N,[Qc,Qp|QOldMoves]):-
	N1 is N-1,
	move(N1,[Qp|QOldMoves]),
	next_move(Qp,Qc),
	\+ member(Qc,[Qp|QOldMoves]).

write_moves(Qs):-
	nl,write('------------------------------------------------'),nl,
	write(Qs).
write_moves(_).
solve:-
	move(64,Qs),
	write_moves(Qs).
	