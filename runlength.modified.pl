runlength([],[]).
runlength(L1,[X|L2]):-first_code(L1,1,X,Rest),runlength(Rest,L2).
runlength(L1,[[N,X]|L2]):-first_code(L1,N,X,Rest),runlength(Rest,L2).
first_code([X|Xs],0,Y,[X|Xs]):-X\=Y.
first_code([X],1,X,[]).
first_code([X|Xs],N1,X,Rest):-first_code(Xs,N,X,Rest), N1 is N+1.