bind_layout_tree(t(a,t(b,nil,nil),t(c,nil,t(d,t(e,nil,nil),nil)))).
bind_layout_tree(t(a,t(b,nil,nil),t(c,nil,t(d,nil,nil)))).
bind_layout_tree(t(a,t(b,nil,nil),t(c,nil,nil))).
bind_layout_tree(t(a,nil,nil)).
bind_layout_tree(t(n,t(k,t(c,t(a,nil,nil),t(h,t(g,t(e,nil,nil),nil),nil)),t(m,nil,nil)),t(u,t(p,nil,t(s,t(q,nil,nil),nil)),nil))).
bind_layout_tree(t(1,t(2,t(3,nil,nil),t(4,nil,nil)),t(5,t(6,nil,nil),t(7,nil,nil)))).

layout_binary_tree1(T,PT):-layout_binary_tree1(T,1,0,PT,_).

layout_binary_tree1(nil,_,PosX_Prev,nil,PosX_Prev).
layout_binary_tree1(t(X,L,R),
		    Depth,
		    PosX_Prev,
		    t(X,PosX,Depth,PT_L,PT_R),
		    PosX_LastUsed_R):-
	DepthChild is Depth+1,
	layout_binary_tree1(L,
			    DepthChild,
			    PosX_Prev,
			    PT_L,
			    PosX_LastUsed_L),
	PosX is PosX_LastUsed_L+1,
	layout_binary_tree1(R,
			    DepthChild,
			    PosX,
			    PT_R,
			    PosX_LastUsed_R).

depth(nil,0).
depth(t(_,L,R),D):-
	depth(L,Dl),
	depth(R,Dr),
	D is max(Dl,Dr)+1.


layout_binary_tree2