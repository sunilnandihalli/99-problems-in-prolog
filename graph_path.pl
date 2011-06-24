bind_graph(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(d,c),
				  e(f,k),e(f,g),e(g,h)])).
bind_graph(graph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])).
bind_graph(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])).
bind_graph(graph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])).

other_end(e(X,Y,_),X,Y).
other_end(e(X,Y,_),Y,X).
other_end(e(X,Y),X,Y).
other_end(e(X,Y),Y,X).
other_end(a(X,Y),X,Y).
other_end(a(X,Y,_),X,Y).

neighbour_nodes(Edges,Node,Neighbours):-
	setof(Neighbour,
	      Edge^(member(Edge,Edges),other_end(Edge,Node,Neighbour)),
	      Neighbours).

graph_path(graph(Nodes,Edges),Start,End,[Start|RestOfPath]):-
	delete(Nodes,Start,Available),
	graph_path(graph(Nodes,Edges),Start,End,RestOfPath,Available).

graph_path(_,Start,Start,[],_).
graph_path(graph(Nodes,Edges),Start,End,[Neighbour|RestOfPath],Available):-
	neighbour_nodes(Edges,Start,Neighbours),
	member(Neighbour,Neighbours),
	member(Neighbour,Available),
	delete(Available,Neighbour,RestOfAvailable),
	graph_path(graph(Nodes,Edges),
		   Neighbour,
		   End,
		   RestOfPath,RestOfAvailable).

test:-bind_graph(graph(Nodes,Edges)),
	write(Nodes),nl,
	write(Edges),nl,
	member(Node1,Nodes),
	member(Node2,Nodes),
	write('------------------------------------------------'),nl,
	write('start : '),write(Node1),nl,
	write('end   : '),write(Node2),nl,
	graph_path(graph(Nodes,Edges),Node1,Node2,Path),
	write('path  : '),write(Path),nl.

% X is adjacent to Y in graph(_,Es).
adjacent(X,Y,graph(_,Es)):-member(e(X,Y),Es).
adjacent(X,Y,graph(_,Es)):-member(e(Y,X),Es).
adjacent(X,Y,graph(_,Es)):-member(e(X,Y,_),Es).
adjacent(X,Y,graph(_,Es)):-member(e(Y,X,_),Es).
adjacent(X,Y,graph(_,Es)):-member(a(Y,X),Es).
adjacent(X,Y,graph(_,Es)):-member(a(Y,X,_),Es).

adjacent_edges(X,Y,Es):-adjacent(X,Y,graph(_,Es)).

graph_cycle(graph(Nodes,Edges),Start,[Start|Xs]):-
	is_graph_path(graph(Nodes,Edges),Xs-[Start]).

is_graph_path(_,L-L).

is_graph_path(Graph,[X1,X2|Xs]-L):-
	adjacent(X2,X1,Graph),
	is_graph_path(Graph,[X2|Xs]-L).
				    	
graph_cycle_new(graph(Nodes,Edges),
		Start,
		[Start|RestOfCycle]):-
	adjacent(Neighbour,Start,graph(Nodes,Edges)),
	graph_path(graph(Nodes,Edges),Neighbour,Start,RestOfCycle).

combination(_,0,[]).
combination(Elements,N,[X|Cs]):-
	N>0,
	append([_,[X],Right],Elements),
	N1 is N-1,
	combination(Right,N1,Cs).

s_tree(graph(Nodes,Edges),graph(Nodes,STreeEdges)):-
	length(Nodes,NNodes),
	NSTreeEdges is NNodes-1,
	combination(Edges,NSTreeEdges,STreeEdges),
	is_tree(graph(Nodes,STreeEdges)).
	
is_connected(graph(Nodes,Edges)):-
	append([Left,[Node],Right],Nodes),
	append([Left,Right],RemainingNodes),
	is_connected(Edges,RemainingNodes,[Node]).

is_connected(_,[],_).
is_connected(Edges,RemainingNodes,[FrontNode|RestOfFrontNodes]):-
	neighbour_nodes(Edges,FrontNode,Neighbours),
	intersection(Neighbours,RemainingNodes,NewFrontNodes),
	ord_subtract(RemainingNodes,NewFrontNodes,NewRemainingNodes),
	append([NewFrontNodes,RestOfFrontNodes],FrontNodes),
	is_connected(Edges,NewRemainingNodes,FrontNodes).
	
is_tree(graph(Nodes,Edges)):-
	is_connected(graph(Nodes,Edges)),
	length(Nodes,NNodes),
	NEdges is NNodes-1,
	length(Edges,NEdges).
	
	