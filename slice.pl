slice([X|_],0,0,[X]).
slice([X|Xs],0,K,[X|Ys]):- K > 0, K1 is K - 1, slice(Xs,0,K1,Ys).
slice([_|Xs],I,K,Ys):- I > 0, I1 is I - 1, K1 is K - 1, slice(Xs,I1,K1,Ys).