mydrop(L1,N,L2):-drop(L1,N,L2,N).
drop([],_,[],_).
drop([_X|Xs],1,Ys,N):-drop(Xs,N,Ys,N).
drop([X|Xs],N,[X|Ys],N):-N1 is N-1,drop(Xs,N1,Ys,N).