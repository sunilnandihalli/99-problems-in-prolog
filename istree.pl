bind(t(a,t(b,nil,nil),nil)).
bind(t(a,t(b,nil,nil))).
is_tree(t(_,nil,nil)):-!.
is_tree(t(_,Y,nil)):-is_tree(Y),!.
is_tree(t(_,nil,Z)):-is_tree(Z),!.       
is_tree(t(_,Y,Z)):-is_tree(Y),is_tree(Z).


%istree(nil).
%istree(t(_,L,R)) :- istree(L), istree(R).
