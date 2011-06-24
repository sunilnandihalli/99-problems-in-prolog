symmetric(nil).
symmetric(t(_,L,R)):-mirror(L,R).

mirror(nil,nil).
mirror(t(_,L1,R1),t(_,L2,R2)):-
	mirror(L1,R2),
	mirror(L2,R1).