% P01 (*): Find the last element of a list

% my_last(X,L) :- X is the last element of the list L
%    (element,list) (?,?)

% Note: last(?Elem, ?List) is predefined

my_last(X,[X]).
my_last(X,[_|L]) :- my_last(X,L).
