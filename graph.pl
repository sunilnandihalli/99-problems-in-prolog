bind_graph(human_readable_graph([b-c, f-c, g-h, d, f-b, k-f, h-g])).
bind_graph(human_readable_graph([s > r, t, u > r, s > u, u > s, v > u])).
bind_graph(human_readable_graph([p>q/9, m>q/7, k, p>m/5])).
%bind_graph(adjacency_list([n(b,[c,f]), n(c,[b,f]), n(d,[]),n(f,[b,c,k]),n(k,[f]),n(g,[h]),n(h,[g])])).
%bind_graph(adjacency_list([n(r,[]),n(s,[r,u]),n(t,[]),n(u,[r]),n(v,[u])])).
%bind_graph(adjacency_list([n(k,[]),n(m,[q/7]),n(p,[m/5,q/9]),n(q,[])])).
%bind_graph(graph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])).
%bind_graph(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])).
%bind_graph(graph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])).


convert(human_readable_graph(X),Y):-
	convert_hrg_g(X,Y).
convert(human_readable_graph(X),Y):-
	convert_hrg_al(X,Y).

edge_end(X-_,X).
edge_end(_-X,X).
edge_end(X>_,X).
edge_end(_>X/_,X):-!.
edge_end(_>X,X):-!.
edge_end(
other_end(X-Y,X,Y).
other_end(X-Y,Y,X).
other_end(X>Y,X,Y).

setof_nodes(human_readable_graph(X),L):-
	setof(Node,Edge^(member(Edge,X),edge_end(Edge,Node)),L).

graph_edge(X-Y,e(X,Y)).
graph_edge(X>Y/N,a(X,Y,N)):-!.
graph_edge(X>Y,a(X,Y)).

setof_graph_edges(human_readable_graph(X),SetOfGraphEdges):-
	setof(GEdge,Edge^(member(Edge,X),graph_edge(Edge,GEdge)),SetOfGraphEdges).

node_neighbours(human_readable_graph(X),Node,NodeNeighbours):-
	setof(OtherEnd,
	      Edge^(member(Edge,X),other_end(Edge,Node,OtherEnd)),
	      NodeNeighbours).

	      
graph_neighbour_nodes(human_readable_graph(X),SetOfNodeNeighbours):-
	setof_nodes(human_readable_graph(X),Nodes),
	setof(n(Node,NodeNeighbours),
	      (member(Node,Nodes),
	       node_neighbours(human_readable_graph(X),
			       Node,
			       NodeNeighbours)),
	      SetOfNodeNeighbours).


convert_hrg_g(X,graph(SetOfNodes,SetOfEdges)):-
	setof_nodes(human_readable_graph(X),SetOfNodes),
	setof_graph_edges(human_readable_graph(X),SetOfEdges).

	
convert_hrg_al(X, adjacency_list(NodeNeighbours)):-
	graph_neighbour_nodes(human_readable_graph(X),NodeNeighbours).
	

convert_al_hrg(X,human_readable_graph(Y)):-


    

    
		   
