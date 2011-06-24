:-ensure_loaded(top95).

mutually_exclusive_with(N,N1):-
	pos(N,R,_,_),
	pos(N1,R,_,_),
	\+ N=N1.

mutually_exclusive_with(N,N1):-
	pos(N,_,C,_),
	pos(N1,R,C,_),
	\+pos(N,R,_,_).

mutually_exclusive_with(N,N1):-
	pos(N,_,_,S),
	pos(N1,R,C,S),
	\+ pos(N,R,_,_),
	\+ pos(N,_,C,_).

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
	
addkey([],_,[]).
addkey([X|Xs],K,[K-(X,K,R,C,S)|XKs]):-
	pos(K,R,C,S),
	K1 is K+1,
	addkey(Xs,K1,XKs).

puzzle_to_array([],[]).
puzzle_to_array([X|Xs],[[X]|AXs]):-
	nonvar(X),
	puzzle_to_array(Xs,AXs).
puzzle_to_array([X|Xs],[[1,2,3,4,5,6,7,8,9]|AXs]):-
	var(X),
	puzzle_to_array(Xs,AXs).
	
create_puzzle(S,P):-
	puzzle_to_array(S,AS),
	addkey(AS,0,ASK),
	list_to_assoc(ASK,P).

setNewId(N1,L1,_N2,L2,N,L):-
	L1<L2,!,
	N is N1,
	L is L1.
setNewId(_N1,_L1,N2,L2,N,L):-
	N is N2,
	L is L2.

select_node_with_fewest_possibilities(P,N,L,[N]):-
	get_assoc(N,P,(V,N,_,_,_)),
	length(V,L).
select_node_with_fewest_possibilities(P,NFewest,LFewest,[NodeId,N1|RestOfRemaingNodes]):-
	get_assoc(NodeId,P,(V,NodeId,_,_,_)),
	length(V,L),
	select_node_with_fewest_possibilities(P,NFewest1,LFewest1,[N1|RestOfRemaingNodes]),
	setNewId(NodeId,L,NFewest1,LFewest1,NFewest,LFewest).

%single_element_node(([_],N,_,_,_),N).
%single_element_nodes(P,L):-
%	setof(N,K^V^(gen_assoc(K,P,V),single_element_node(V,N)),L).


single_element_nodes(_P,[],[],[]):-!.
single_element_nodes(P,[N|Ns],[N|RestOfRIn],ROut):-
	get_assoc(N,P,([_],_,_,_,_)),!,
	single_element_nodes(P,Ns,RestOfRIn,ROut).
single_element_nodes(P,Ns,[N|RestOfRIn],[N|RestOfRout]):-
	single_element_nodes(P,Ns,RestOfRIn,RestOfRout).

trim_puzzle(P,P,[],[]):-!.
trim_puzzle(P,TrimP,RemainingIn,RemainingOut):-
	single_element_nodes(P,TrimNodes,RemainingIn,NewRemainingIn),
	\+ TrimNodes = [],!,
	set_nodes_in_list(P,TrimNodes,TrimP1),
	trim_puzzle(TrimP1,TrimP,NewRemainingIn,RemainingOut).
trim_puzzle(P,P,RemainingIn,RemainingIn).

writeSpace(0):-!.
writeSpace(N):-
	write(' '),
	N1 is N-1,
	writeSpace(N1).

writeList([]):-!.
writeList([V|Vs]):-
	write(V),
	writeList(Vs).

printElem((V,_,_,_,_),DW):-
	length(V,L),
	Bf is DW-L,
	FBF is Bf//2,
	BBF is Bf - FBF,
	writeSpace(FBF),
	writeList(V),
	writeSpace(BBF).
	

display_row(P,RowId,DisplayWidth):-
	pos(N,RowId,_ColId,_),
	get_assoc(N,P,V),
	printElem(V,DisplayWidth),
	fail.

display_row(_,_,_):-
	nl.
	
display_puzzle(P):-
	find_length_of_longest_val(P,Lmax),
	DisplayWidth is Lmax+2,
	display_puzzle(P,DisplayWidth).
display_puzzle(P,DisplayWidth):-
	between(0,8,RowId),
	display_row(P,RowId,DisplayWidth),
	fail.
display_puzzle(_,_):-
	nl.
	
find_length_of_longest_val(P,Lmax):-
	assoc_to_list(P,PL),
	find_max_length(PL,Lmax).

find_max_length([],0).
find_max_length([_-(X,_,_,_,_)|Xs],Lmax):-
	length(X,L),
	find_max_length(Xs,LsMax),
	Lmax is max(L,LsMax).

set_nodes_in_list(P,[],P).
set_nodes_in_list(P,[N|Ns],P2):-
	get_assoc(N,P,(Vs,_Id,_RId,_CId,_SId)),
	member(V,Vs),
	set(P,N,V,P1),
	set_nodes_in_list(P1,Ns,P2).

eliminate_from_mutually_exclusive_ents(P,[],_,P).
eliminate_from_mutually_exclusive_ents(P,[NEx|NExs],V,P2):-
	get_assoc(NEx,P,(OldVal,N,R,C,S)),
	delete(OldVal,V,NewVal),
	get_assoc(NEx,P,_,P1,(NewVal,N,R,C,S)),
	eliminate_from_mutually_exclusive_ents(P1,NExs,V,P2).

eliminate(P,N,V,P1):-
	setof(NEx,mutually_exclusive_with(N,NEx),NExs),
	eliminate_from_mutually_exclusive_ents(P,NExs,V,P1).	

set(P,N,V,P1):-
	eliminate(P,N,V,P_RCS),
	pos(N,R,C,S),
	get_assoc(N,P_RCS,_,P1,([V],N,R,C,S)).

solve_sudoku(P,P,[],[]).
solve_sudoku(P,Pout,RemainingIn,RemainingOut):-
	select_node_with_fewest_possibilities(P,N,_L,RemainingIn),
	get_assoc(N,P,(TV,_,_,_,_)),
	member(V,TV),
	set(P,N,V,TP),
	trim_puzzle(TP,TP1,RemainingIn,NewRemainingIn),
%	write(trim),nl,
%	display_puzzle(TP1),
	solve_sudoku(TP1,Pout,NewRemainingIn,RemainingOut).

solve(N):-
	top95(N,S),
	create_puzzle(S,P),
	assoc_to_keys(P,Remaining),
	solve_sudoku(P,Pout,Remaining,[]),
	write(solution),nl,
	display_puzzle(Pout).


%eliminate(P,N,P1):-
%	eliminate_imposibble(P).

%set(Val,N,Sol,NewSol):-
%	pos(N,R,C,S),
%	get_assoc(N,P,OldVal,P1,Val),
%	eliminate_val.
	
	