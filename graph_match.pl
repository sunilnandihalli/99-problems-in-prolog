bind_graph1(graph([a,b,c],[e(a,b),e(a,c)])).
bind_graph1(graph([a,b,c,d,e,f,g],[e(a,b),e(a,c),e(a,d),e(b,e),e(d,f),e(f,g)])).
bind_graph1(graph([a,b],[e(a,b)])).
bind_graph1(graph([a],[])).
bind_graph1(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(d,c),
				   e(f,k),e(f,g),e(g,h)])).
bind_graph1(graph([k,m,p,q],[e(m,p,7),e(p,m,5),e(p,q,9)])).
bind_graph1(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])).
bind_graph1(graph([r,s,t,u,v],[e(s,r),e(s,u),e(u,r),e(u,s),e(v,u)])).

bind_graph2(graph([aa,bb,cc],[e(aa,bb),e(aa,cc)])).
bind_graph2(graph([aa,bb,cc,dd,ee,ff,gg],[e(aa,bb),e(aa,cc),
					  e(aa,dd),e(bb,ee),
					  e(dd,ff),e(ff,gg)])).
bind_graph2(graph([c,d],[e(c,d)])).
bind_graph2(graph([c],[])).
bind_graph2(graph([kk,bb,ff,dd,gg,cc,hh],[e(gg,hh),e(bb,cc),e(bb,ff),
					  e(cc,ff),e(dd,cc),
					  e(ff,kk),e(ff,gg)])).
bind_graph2(graph([m,p,k,q],[e(m,p,7),e(p,q,9),e(p,m,5)])).
bind_graph2(graph([g,k,b,d,f,c,h],[e(b,f),e(f,k),e(b,c),e(g,h),e(c,f)])).
bind_graph2(graph([r,v,t,u,s],[e(s,r),e(u,r),e(u,s),e(v,u),e(s,u)])).

bind_edges([e(a,b),e(a,c),e(b,c),e(d,e),e(b,d),e(a,d)]).
bind_nodes([a,b,c,d,e,f]).
other_end(e(X,Y,_),X,Y):-!.
other_end(e(X,Y,_),Y,X):-!.
other_end(e(X,Y),X,Y):-!.
other_end(e(X,Y),Y,X):-!.

neighbour_nodes(Edges,Node,Neighbours):-
	nonvar(Edges),
	nonvar(Node),
	setof(Neighbour,
	      Edge^(member(Edge,Edges),other_end(Edge,Node,Neighbour)),
	      Neighbours),!.
neighbour_nodes(_,_,[]).

pattern_match(graph(Nodes1,Edges1),graph(Nodes2,Edges2),Match):-
	pattern_match(graph(Nodes1,Edges1),graph(Nodes2,Edges2),
		      [], % front
		      Nodes1,Nodes2, % remaining nodes in the two graphs
		      Match).

pattern_match(_,_,[],[],[],[]).

pattern_match(_,_,
	      [],
	      [TryN1|RestOfRemainingNs1],RemainingNs2,
	      [m(TryN1,TryN2)|RestOfMatch]):-
	select(TryN2,RemainingNs2,RestOfRemainingNs2),
	pattern_match(_,_,
		      [m(TryN1,TryN2)],
		      RestOfRemainingNs1,RestOfRemainingNs2,
		      RestOfMatch).

pattern_match(graph(_,Edges1),graph(_,Edges2),
	      [m(X1,X2)|RestOfFront],
	      RemainingNodes1,RemainingNodes2,
	      Match):-
	neighbour_nodes(Edges1,X1,Neighbours1),
	neighbour_nodes(Edges2,X2,Neighbours2),
	intersection(RemainingNodes1,Neighbours1,NewNodesInFront1),
	intersection(RemainingNodes2,Neighbours2,NewNodesInFront2),
	permute(NewNodesInFront1,NewNodesInFront2,CurrentMatch),
	append(CurrentMatch,RestOfFront,NewFront),
	ord_subtract(RemainingNodes1,NewNodesInFront1,RestOfRemainingNodes1),
	ord_subtract(RemainingNodes2,NewNodesInFront2,RestOfRemainingNodes2),
	pattern_match(graph(_,Edges1),graph(_,Edges2),
		      NewFront,
		      RestOfRemainingNodes1,RestOfRemainingNodes2,
		      RestOfMatch),
	
	append(CurrentMatch,RestOfMatch,Match).

permute([],[],[]).
permute([X1|X1s],L2,[m(X1,X2)|Ms]):-
	select(X2,L2,X2s),
	permute(X1s,X2s,Ms).

test:-
	bind_graph1(X1),bind_graph2(X2),
	write(X1),nl,write(X2),nl,
	pattern_match(X1,X2,M),
	write(M),nl,fail.
