tell_order(A,B):-
	compare('<',A,B),
	write(less_than),nl.

tell_order(A,B):-
	compare('=',A,B),
	write(equal_to),nl.

tell_order(A,B):-
	compare('>',A,B),
	write(greater_than),nl.