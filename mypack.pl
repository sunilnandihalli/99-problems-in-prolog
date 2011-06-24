stransfer([X|Xs],[X|Ys]):-stransfer(Xs,[X,X|Ys]).
stransfer([X|Xs],[]):-stransfer(Xs,[X]).
stransfer(X,Y).