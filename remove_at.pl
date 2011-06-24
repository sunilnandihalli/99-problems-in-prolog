sremove_at(X,[X|Xs],1,Xs).
sremove_at(X,[Y|Xs],K,[Y|R]):- K1 is K-1,sremove_at(X,Xs,K1,R).