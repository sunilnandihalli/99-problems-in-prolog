% P02 (*): Find the last but one element of a list

% last_but_one(X,L) :- X is the last but one element of the list L
%    (element,list) (?,?)

last_but_one(X,[X,_]).
last_but_one(X,[_,Y|Ys]) :- last_but_one(X,[Y|Ys]).
