graph_bind(graph([a,b],[e(a,b)])).
graph_bind(graph([a,b,c],[e(a,b),e(a,c)])).
graph_bind(graph([a,b,c,d,e,f,g],
		 [e(a,b),e(a,d),e(a,g),
		  e(b,c),e(b,e),e(e,f)])).
graph_bind(graph([a,b,c,d,e,f,g,h,i,k,m,n,p,q],
		 [e(a,b),e(a,c),e(a,g),e(a,h),e(a,i),
		  e(c,d),e(c,e),e(c,f),
		  e(d,k),
		  e(e,q),
		  e(m,q),
		  e(n,q),e(n,p)])).

check_edge(NodeIdPairs,e(A,B),e(A,B)-N):-
	member(A-Na,NodeIdPairs),
	member(B-Nb,NodeIdPairs),
	N is abs(Na-Nb).

all_different(L):-
	length(L,N),
	all_different(L,N).

all_different(_,0).
all_different(L,N):-
	member(N,L),
	N1 is N-1,
	all_different(L,N1).



	
solve(P,PEs):-
	graph_bind(graph(Ns,Es)),
	length(Ns,N),
	setof(I,between(1,N,I),Is),
	permutation(Is,PermIs),
	pairs_keys_values(P,Ns,PermIs),
	Pred =.. [check_edge , P],
	maplist(Pred,Es,PEs),
	pairs_keys_values(PEs,_,Values),
	all_different(Values).
