bind_graph(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(d,c),
				  e(f,k),e(f,g),e(g,h)])).
bind_graph(graph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])).
bind_graph(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])).
bind_graph(graph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])).

degree(graph(_,Es),Node,Degree):-
	neighbouring_nodes(Es,Node,NeighbouringNodes),
	length(NeighbouringNodes,Degree).

neighbouring_nodes(Es,N,NNs):-
	setof(NN,bind_neighbour(N,Es,NN),NNs),!.
neighbouring_nodes(_,_,[]).

bind_neighbour(N,Es,NN):-
	member(e(N,NN),Es).

bind_neighbour(N,Es,NN):-
	member(e(NN,N),Es).

bind_neighbour(N,Es,NN):-
	member(a(N,NN),Es).

bind_neighbour(N,Es,NN):-
	member(a(N,NN,_),Es).

degree_attached_nodes(_,[],[]).
degree_attached_nodes(Es,[N|Ns],[p(D,N)|DNs]):-
	degree(graph(_,Es),N,D),
	degree_attached_nodes(Es,Ns,DNs).

detach_degree_from_nodes([],[]).
detach_degree_from_nodes([p(_,N)|DNs],[N|Ns]):-
	detach_degree_from_nodes(DNs,Ns).

reorder_nodes_in_decreasing_degree_order(graph(Ns,Es),graph(NsDecreasingOrder,Es)):-
	degree_attached_nodes(Es,Ns,DNs),
	write(DNs),nl,
	sort(DNs,SortedDNs),
	write(SortedDNs),nl,
	detach_degree_from_nodes(SortedDNs,NsIncreasingOrder),
	reverse(NsIncreasingOrder,NsDecreasingOrder).

test:-
	bind_graph(X),
	reorder_nodes_in_decreasing_degree_order(X,NX),
	write(X),nl,
	write(NX),nl,
	fail.