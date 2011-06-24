:- ensure_loaded(permute).


calc_list(L,C,SplitL):-
	calc_list(L,C,SplitL,0).

calc_list([],[],[],_).
calc_list([X|Xs],[C|Cs],[N1-N2|PNs],K):-
	N1 is K+C,
	N2 is N1+X,
	K1 is K+X,
	calc_list(Xs,Cs,PNs,K1).

nono_combination_row(L,NL,SplitL):-
	sumlist(L,Sum),
	length(L,K),
	N is NL-Sum,
	bagof(I,between(0,N,I),Is),
	combination(K,Is,C),
	calc_list(L,C,SplitL).

is_full([N1-N2|_],X):-
	X<N2,!,
	X>=N1.
is_full([_|PNs],X):-
	is_full(PNs,X).


nono_combination_col_noblank([Row|Rows],ColId,X,RowsRemaining):-
	is_full(Row,ColId),
	nono_combination_col_noblank(Rows,ColId,X1,RowsRemaining),
	X is X1+1.
nono_combination_col_noblank([Row|Rows],ColId,X,Rows):-
	\+ is_full(Row,ColId),
	X is 0.
nono_combination_col_noblank([],_,0,[]).

nono_combination_col([],_,[]).
nono_combination_col([Row|Rows],ColId,[X|Xs]):-
	is_full(Row,ColId),!,
	nono_combination_col_noblank(Rows,ColId,X1,RowsRemaining),
	X is X1+1,
	nono_combination_col(RowsRemaining,ColId,Xs).
nono_combination_col([_|Rows],ColId,Xs):-
	nono_combination_col(Rows,ColId,Xs).


chk_cols(Cs,Rows):-
	chk_cols(Cs,0,Rows).


chk_cols([C|Cs],ColId,Rows):-!,
	nono_combination_col(Rows,ColId,C),
	ColId1 is ColId+1,
	chk_cols(Cs,ColId1,Rows).
chk_cols([],_,_).

gen_rows([],_,[]).
gen_rows([R|Rs],Rmax,[CR|CRs]):-
	nono_combination_row(R,Rmax,CR),
	gen_rows(Rs,Rmax,CRs).

display_nonogram(A,B):-
	write(nonogram),nl,
	place('-',0,B),nl,
	display_nonogram_h(A,B),
	place('-',0,B),nl.

display_nonogram_h([],_).
display_nonogram_h([Row|Rows],Rmax):-
	display_row(Row,Rmax,0),nl,
	display_nonogram_h(Rows,Rmax).

place(X,From,To):-
	From<To,
	write(X),
	From1 is From+1,
	place(X,From1,To).
place(_,To,To).

display_row([],Rmax,Rmax):-!.
display_row([N1-N2|Rs],Rmax,CurColId):-
	place('.',CurColId,N1),
	place('X',N1,N2),
	display_row(Rs,Rmax,N2).
display_row([],Rmax,CurColId):-
	place('.',CurColId,Rmax).

solve_nonogram(Rows,Cols,CRs):-
	length(Cols,Rmax),
	gen_rows(Rows,Rmax,CRs),
	chk_cols(Cols,CRs).

bind_nono(1,
	  [[3],[2,1],[3,2],[2,2],[6],[1,5],[6],[1],[2]],
	  [[1,2],[3,1],[1,5],[7,1],[5],[3],[4],[3]]).
bind_nono(2,
	  [[2],[2]],
	  [[2],[2]]).
bind_nono(3,
	  [[1]],
	  [[1]]).
bind_nono(4,
	  [[3],[1,1],[3]],
	  [[3],[1,1],[3]]).
bind_nono(5,
	  [[3],[1],[3],[1],[3]],
	  [[3,1],[1,1,1],[1,3]]).

bind_nono(6,
	  [[3,1], [2,4,1], [1,3,3], [2,4], [3,3,1,3], [3,2,2,1,3],
	   [2,2,2,2,2], [2,1,1,2,1,1], [1,2,1,4], [1,1,2,2], [2,2,8],
	   [2,2,2,4], [1,2,2,1,1,1], [3,3,5,1], [1,1,3,1,1,2],
	   [2,3,1,3,3], [1,3,2,8], [4,3,8], [1,4,2,5], [1,4,2,2],
	   [4,2,5], [5,3,5], [4,1,1], [4,2], [3,3]],
	  [[2,3], [3,1,3], [3,2,1,2], [2,4,4], [3,4,2,4,5], [2,5,2,4,6],
	   [1,4,3,4,6,1], [4,3,3,6,2], [4,2,3,6,3], [1,2,4,2,1], [2,2,6],
	   [1,1,6], [2,1,4,2], [4,2,6], [1,1,1,1,4], [2,4,7], [3,5,6],
	   [3,2,4,2], [2,2,2], [6,3]]).

bind_nono(7,
	  [[5], [2,3,2], [2,5,1], [2,8], [2,5,11], [1,1,2,1,6], [1,2,1,3],
	   [2,1,1], [2,6,2], [15,4], [10,8], [2,1,4,3,6], [17], [17],
	   [18], [1,14], [1,1,14], [5,9], [8], [7]],
	  [[5], [3,2], [2,1,2], [1,1,1], [1,1,1], [1,3], [2,2], [1,3,3],
	   [1,3,3,1], [1,7,2], [1,9,1], [1,10], [1,10], [1,3,5], [1,8],
	   [2,1,6], [3,1,7], [4,1,7], [6,1,8], [6,10], [7,10], [1,4,11],
	   [1,2,11], [2,12], [3,13]]).


possible(Rows,Cols,Sum):-
	maplist(sumlist,Rows,RL),
	sumlist(RL,Sum),
	maplist(sumlist,Cols,CL),
	sumlist(CL,Sum).

test_gen_row(1):-
	bind_nono(Rows,Cols),
	length(Cols,Rmax),
	gen_rows(Rows,Rmax,CRs),
	L is Rmax+1,
	\+ length(CRs,L),
	length(CRs,ActualR),
	write([rmax,Rmax,actualr,ActualR]),nl,
	write(CRs),nl,
	write(displaying),nl,
	display_nonogram(CRs,Rmax),nl.

test_col(N):-
	bind_nono(N,Rows,Cols),
	length(Cols,Rmax),
	gen_rows(Rows,Rmax,CRs),
	display_nonogram(CRs,Rmax),
	Rmax1 is Rmax -1,
	between(0,Rmax1,CId),
	nono_combination_col(CRs,CId,C),
	write([cid,CId,c,C]),nl.

test_nono(N):-
	bind_nono(N,Rows,Cols),
	write([rows,Rows,cols,Cols]),nl,
	length(Cols,Rmax),
	solve_nonogram(Rows,Cols,CRs),
	display_nonogram(CRs,Rmax).