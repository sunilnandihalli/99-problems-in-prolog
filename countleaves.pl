count_leaves(nil,0).
count_leaves(t(_,L,R),N):-count_leaves(L,N1),count_leaves(R,N2),N is N1+N2+1.