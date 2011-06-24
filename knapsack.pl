:-ensure_loaded(library(clpr)).
:-ensure_loaded(library(simplex)).
knapsack_constrain(S) :-
	gen_state(S0),
	constraint([6*x(1), 4*x(2)] =< 8, S0, S1),
	constraint([x(1)] =< 1, S1, S2),
	constraint([x(2)] =< 2, S2, S).

knapsack(S) :-
	knapsack_constrain(S0),
	maximize([7*x(1), 4*x(2)], S0, S).
