% P67 (**) A string representation of binary trees

% Most elegant solution using difference lists.

tree_string(T,S) :- nonvar(T), tree_dlist(T,L-[]), !, atom_chars(S,L). 
tree_string(T,S) :- nonvar(S), atom_chars(S,L), tree_dlist(T,L-[]).

% tree_dlist/2 does the trick in both directions!

tree_dlist(nil,L-L).
tree_dlist(t(X,nil,nil),L1-L2) :- 
   letter(X,L1-L2).
tree_dlist(t(X,Left,Right),L1-L7) :- 
   letter(X,L1-L2), 
   symbol('(',L2-L3),
   tree_dlist(Left,L3-L4),
   symbol(',',L4-L5),
   tree_dlist(Right,L5-L6),
   symbol(')',L6-L7).

symbol(X,[X|Xs]-Xs).

letter(X,L1-L2) :- symbol(X,L1-L2), char_type(X,alpha).
