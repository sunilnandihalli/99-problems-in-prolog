:- ensure_loaded(p87).
bind_graph(graph([a,b,c,d,e,f,g,h],
		 [e(a,b),e(a,d),e(b,c),e(b,e),
		  e(c,e),e(d,e),e(d,f),e(d,g),
		  e(e,h),e(f,g),e(g,h)])).
bind_graph(graph([a,b,c,d,e,f,g,h],[e(a,b,5),e(a,d,3),e(b,c,2),e(b,e,4),
				    e(c,e,6),e(d,e,7),e(d,f,4),e(d,g,3),
				    e(e,h,5),e(f,g,4),e(g,h,1)])).
bind_graph(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(d,c),
				  e(f,k),e(f,g),e(g,h)])).
bind_graph(graph([k,m,p,q],[a(m,p,7),a(p,m,5),a(p,q,9)])).
bind_graph(graph([b,c,d,f,g,h,k],[e(b,c),e(b,f),e(c,f),e(f,k),e(g,h)])).
bind_graph(graph([r,s,t,u,v],[a(s,r),a(s,u),a(u,r),a(u,s),a(v,u)])).

test:-
	bind_graph(graph(Ns,Es)),
	write('-------------------------------------'),nl,
	write(graph(Ns,Es)),nl,
	member(N,Ns),
	depth_first_order(graph(Ns,Es),N,Seq),
	write(Seq),nl,fail.