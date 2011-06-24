:-ensure_loaded(permute).
different_by_one_bit(A,B):-I is A xor B,has_exactly_one_bit_on(I).
has_exactly_one_bit_on(I):-I>0,K is I/\1,K>0,I1 is I>>1,I1=:=0.
has_exactly_one_bit_on(I):-I>0,K is I/\1,K =:= 0,I1 is I>>1,has_exactly_one_bit_on(I1).

upto(0,[0]).
upto(N,[N|Xs]):-N>0,N1 is N-1,upto(N1,Xs).

max(NBits,Val):-Val is 2^NBits-1.

graycode(NBits,C):-
	max(NBits,Max),
	upto(Max,P),
	length(P,Length),
	permutation(Length,P,C),
	is_gray(C).
is_gray([]).
is_gray([X1,X2|Xs]):-
	different_by_one_bit(X1,X2),
	is_gray([X2|Xs]).
is_gray([_]).
