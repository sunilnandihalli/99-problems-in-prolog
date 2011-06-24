sunil_reverse(List,ReverseList):-sunil_reverse(List,ReverseList,[]).
sunil_reverse([],ReverseList,ReverseList):-!.
sunil_reverse([X|Xs],ReverseList,Accumulator):-sunil_reverse(Xs,ReverseList,[X|Accumulator]).
