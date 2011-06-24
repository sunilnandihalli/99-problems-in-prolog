% P14 (*): Duplicate the elements of a list

% dupli(L1,L2) :- L2 is obtained from L1 by duplicating all elements.
%    (list,list) (?,?)

dupli([],[]).
dupli([X|Xs],[X,X|Ys]) :- dupli(Xs,Ys).
