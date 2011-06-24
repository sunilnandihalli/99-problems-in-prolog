eliminate_consecutive_duplicates(L1,L2):-eliminate_consecutive_duplicates(L1,L2,[]).
eliminate_consecutive_duplicates([],L2,Acc):-reverse(Acc,L2).
eliminate_consecutive_duplicates([X,X|Rx],L2,Acc):-eliminate_consecutive_duplicates([X|Rx],L2,Acc).
eliminate_consecutive_duplicates([X|Rx],L2,Acc):-eliminate_consecutive_duplicates(Rx,L2,[X|Acc]).