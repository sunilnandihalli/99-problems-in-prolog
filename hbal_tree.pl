:-ensure_loaded(cbal_tree).

max_min_depth(t(_,R,L),D):-
	max_min

is_height_balanced_tree(t(_,R,L)):-
	max_min_depth(R,DrMin,DrMax),
	max_min_depth(L,DlMin,DlMax),
	\+ abs(min(DrMin,DlMin)-Dl) > 1.

hbal_tree(4,T):-