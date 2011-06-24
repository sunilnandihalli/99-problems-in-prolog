% P19 (**): Rotate a list N places to the left 

% rotate(L1,N,L2) :- the list L2 is obtained from the list L1 by 
%    rotating the elements of L1 N places to the left.
%    Examples: 
%    rotate([a,b,c,d,e,f,g,h],3,[d,e,f,g,h,a,b,c])
%    rotate([a,b,c,d,e,f,g,h],-2,[g,h,a,b,c,d,e,f])
%    (list,integer,list) (+,+,?)

:- ensure_loaded(p17).

rotate(L1,N,L2) :- N >= 0, 
   length(L1,NL1), N1 is N mod NL1, rotate_left(L1,N1,L2).
rotate(L1,N,L2) :- N < 0,
   length(L1,NL1), N1 is NL1 + (N mod NL1), rotate_left(L1,N1,L2).

rotate_left(L,0,L).
rotate_left(L1,N,L2) :- N > 0, split(L1,N,S1,S2), append(S2,S1,L2).




