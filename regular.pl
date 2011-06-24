bind_graph1(1,graph([a,b,c],[e(a,b),e(a,c)])).
bind_graph1(2,graph([a,b,c,d,e,f,g],[e(a,b),e(a,c),e(a,d),e(b,e),e(d,f),e(f,g)])).
bind_graph1(3,graph([a,b],[e(a,b)])).
bind_graph1(4,graph([a],[])).
bind_graph1(5,graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(d,c),
				     e(f,k),e(f,g),e(g,h)])).
bind_graph1(6,graph([k,m,p,q],[e(m,p,7),e(p,m,5),e(p,q,9)])).
bind_graph1(7,graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])).
bind_graph1(8,graph([r,s,t,u,v],[e(s,r),e(s,u),e(u,r),e(u,s),e(v,u)])).

bind_graph2(1,graph([aa,bb,cc],[e(aa,bb),e(aa,cc)])).
bind_graph2(2,graph([aa,bb,cc,dd,ee,ff,gg],[e(aa,bb),e(aa,cc),
					    e(aa,dd),e(bb,ee),
					    e(dd,ff),e(ff,gg)])).
bind_graph2(3,graph([c,d],[e(c,d)])).
bind_graph2(4,graph([c],[])).
bind_graph2(5,graph([kk,bb,ff,dd,gg,cc,hh],[e(gg,hh),e(bb,cc),e(bb,ff),
					    e(cc,ff),e(dd,cc),
					    e(ff,kk),e(ff,gg)])).
bind_graph2(6,graph([m,p,k,q],[e(m,p,7),e(p,q,9),e(p,m,5)])).
bind_graph2(7,graph([g,k,b,d,f,c,h],[e(b,f),e(f,k),e(b,c),e(g,h),e(c,f)])).
bind_graph2(8,graph([r,v,t,u,s],[e(s,r),e(u,r),e(u,s),e(v,u),e(s,u)])).

change_edge_rep(e(X,Y),X-Y).
change_edge_rep(e(X,Y,_),X-Y).


		
isomorph(graph(N1,E1),graph(N2,E2),L):-
	maplist(change_edge_rep,E1,E1L),
	maplist(change_edge_rep,E2,E2L),
	append(N1,E1L,G1),
	append(N2,E2L,G2),
	unify_graph(G1,G2,L).

unify_graph([],[],_).
unify_graph([G1|G1s],G2L,L):-
	select(G2,G2L,G2s),
	unify(G1,G2,L),
	unify_graph(G1s,G2s,L).

unify(X1-X2,Y1-Y2,L):-
	bind(X1,Y1,L),bind(X2,Y2,L).
unify(X1-X2,Y1-Y2,L):-
	bind(X1,Y2,L),bind(X2,Y1,L).
unify(X,Y,L):-
	\+ X=_-_,\+ Y=_-_,
	bind(X,Y,L).

bind(X,Y,L):-
	memberchk(X-Y0,L),nonvar(Y0),!,Y = Y0.
bind(X,Y,L):-
	memberchk(X0-Y,L),X = X0.

combination(_,0,[]).
combination(L,K,[C|Cs]):-
	append([_,[C],R],L),
	K1 is K-1,
	combination(R,K1,Cs).

test(N):-
	bind_graph1(N,G1),
	bind_graph2(N,G2),
	isomorph(G1,G2,L),
	maplist(writeln,[G1,G2,L]).

edge_rep([X,Y],e(X,Y)).

all_possible_edges(Nodes,Edges):-
	bagof(E,combination(Nodes,2,E),Es),
	maplist(edge_rep,Es,Edges).
all_possible_edges([_],[]).

power_set([],[]).
power_set([X|Xs],[X|PS]):-
	power_set(Xs,PS).
power_set([_|Xs],PS):-
	power_set(Xs,PS).

gen_graph(Ns,Es,graph(Ns,PEs)):-
	power_set(Es,PEs).

generate_graphs(NNodes,G):-
	length(Nodes,NNodes),
	maplist(gensym(n),Nodes),
	all_possible_edges(Nodes,Edges),
	gen_graph(Nodes,Edges,G).

mneighbours(_,[],[]).
mneighbours(N,[e(N,N2)|Es],[N2|Neighbours]):-!,
	mneighbours(N,Es,Neighbours).
mneighbours(N,[e(N1,N)|Es],[N1|Neighbours]):-!,
	mneighbours(N,Es,Neighbours).
mneighbours(N,[_|Es],Neighbours):-!,
	mneighbours(N,Es,Neighbours).

remove_isomorphic_entries([],[]).
remove_isomorphic_entries([G|Gs],[G|NonIsomorphicGs]):-
	(bagof(NIG,(member(NIG,Gs),\+ isomorph(G,NIG,_)),NIGs),!;NIGs=[]),
	remove_isomorphic_entries(NIGs,NonIsomorphicGs).

generate_non_isomorphic_k_regular_graphs(N,K,NonIsomorphicG):-
	bagof(G,generate_k_regular_graphs(N,K,G),Gs),
	remove_isomorphic_entries(Gs,NonIsomorphicGs),
	member(NonIsomorphicG,NonIsomorphicGs).

valency(graph(_,Es),V,N):-
	mneighbours(N,Es,Neighbours),
	length(Neighbours,V).

is_graph_k_regular(graph(Ns,Es),K):-
	maplist(valency(graph(Ns,Es),K),Ns).

generate_k_regular_graphs(NNodes,K,G):-
	generate_graphs(NNodes,G),
	is_graph_k_regular(G,K).