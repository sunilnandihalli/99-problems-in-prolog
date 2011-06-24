split(N,N1,N2):-N1 is (N-1)//2, N2 is (N-1)-N1.
split(N,N1,N2):-N2 is (N-1)//2, N1 is (N-1)-N2,\+N1=N2.

mybalanced_tree(0,nil):-!.
mybalanced_tree(N,t(x,L,R)):-
	split(N,N1,N2),
	mybalanced_tree(N1,L),
	mybalanced_tree(N2,R).
	

