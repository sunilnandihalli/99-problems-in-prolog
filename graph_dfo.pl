other_end(e(N,NN,_),N,NN).
other_end(e(NN,N,_),N,NN).
other_end(e(N,NN),N,NN).
other_end(e(NN,N),NN,N).

neighbour(N,Es,NN):-
	member(E,Es),
	other_end(E,N,NN).

neighbours(N,Es,NNs):-
	setof(NN,E^(member(E,Es),other_end(E,N,NN)),NNs).

dfo(graph(Ns,Es),Start,Seq):-
	dfo_dl(Es,
	       [Start],_FrontOut,
	       _,Seq,
	       Ns,_RemainingNodesOut).

symbol(X,[X|L]-L).
dfo(Es,
    [],[],
    SeqIn,SeqIn,
    [],[]):-!.
dfo(Es,
    [],FrontNodesOut,
    SeqIn,SeqOut,
    RemainingNodesIn,RemainingNodesOut):-
	select(N,RemainingNodesIn,RemainingNodesInNew),
	dfo(Es,
	    [N],FrontNodesOut,
	    SeqIn,SeqOut,
	    RemainingNodesInNew,RemainingNodesOut).
addNodeToSequence(	
dfo(Es,
    [FrontNode|FrontNodesIn],FrontNodesOut,
    SeqIn,SeqOut,
    RemainingNodesIn,RemainingNodesOut):-
	addNodeToSequence(FrontNode,SeqIn,SeqInNew),
	neighbours(FrontNode,Es,Neighbours),
	subtract(Neighbours,SeqInNew,NewFrontNodes),
	append(NewFrontNodes,FrontNodesIn,FrontNodesInNew),
	dfo(Es,
	    FrontNodesInNew,FrontNodesOut,
	    SeqInNew,SeqOut,
	    RemainingInNew,RemainingNodesOut),