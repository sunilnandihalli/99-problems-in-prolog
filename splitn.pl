split(Xs,0,[],Xs).
split([X|Xs],N,[X|Ys],L2):-N1 is N-1,split(Xs,N1,Ys,L2).