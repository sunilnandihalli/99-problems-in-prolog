:- use_module(library(clpfd)).
:- ensure_loaded(top95).

square_id(S,_,R,C):-
	S_CID is C//3,
	S_RID is R//3,
	S is S_RID * 3 + S_CID.

pos(N,R,C,S):-
	nonvar(N),!,
	R is N//9,
	C is N mod 9,
	square_id(S,N,R,C).

pos(N,R,C,S):-
	nonvar(R),
	nonvar(C),!,
	N is R*9+C,
	square_id(S,N,R,C).

pos(N,R,C,S):-
	nonvar(R),!,
	between(0,8,C),
	N is R*9+C,
	square_id(S,N,R,C).

pos(N,R,C,S):-
	nonvar(C),!,
	between(0,8,R),
	N is R*9+C,
	square_id(S,N,R,C).

pos(N,R,C,S):-
	nonvar(S),!,
	R0 is S//3*3,
	C0 is (S mod 3)*3,
	between(0,2,R1),
	R is R0+R1,
	between(0,2,C1),
	C is C0+C1,
	N is R*9+C.

group_all_rows(P):-
	setof(R,between(1,9,R),Rs),
	group_rows(P,Rs).

group_all_cols(P):-
	setof(C,between(1,9,C),Cs),
	group_cols(P,Cs).

group_all_squares(P):-
	setof(S,between(1,9,S),Ss),
	group_squares(P,Ss).

group_rows(_,[]).
group_rows(P,[R|Rs]):-
	group_row(P,R),
	group_rows(P,Rs).

group_row(P,R):-
	setof(X,N^(pos(N,R,_,_),nth0(N,P,X)),Xs),
	all_different(Xs).

group_cols(_,[]).
group_cols(P,[C|Cs]):-
	group_col(P,C),
	group_cols(P,Cs).

group_col(P,C):-
	setof(X,N^(pos(N,_,C,_),nth0(N,P,X)),Xs),
	all_different(Xs).

group_squares(_,[]).
group_squares(P,[S|Ss]):-
	group_square(P,S),
	group_squares(P,Ss).

group_square(P,S):-
	setof(X,N^(pos(N,_,_,S),nth0(N,P,X)),Xs),
	all_different(Xs).

sudoku_solve(P):-
	P ins 1..9,

	group_all_rows(P),
	group_all_cols(P),
	group_all_squares(P),
	label(P).
	