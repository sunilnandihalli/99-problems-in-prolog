while_i_less_than_3(I):-
	I<3,!,
	write([i,I]),nl,
	I1 is I+1,
	while_i_less_than_3(I1).
while_i_less_than_3(_):-
	write('while loop done'),nl.
	       