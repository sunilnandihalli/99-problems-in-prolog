identifier([X|Xs]):-
	char_type(X,alpha),
	intermediate(Xs).

is_valid_intermediate_entry('-').
is_valid_intermediate_entry(X):-char_type(X,alnum).

is_valid_last_entry(X):-char_type(X,alnum).


intermediate([]).
intermediate([X,Y]):-!,
	is_valid_intermediate_entry(X),
	is_valid_last_entry(Y).
intermediate([X|Xs]):-
	is_valid_intermediate_entry(X),
	intermediate(Xs).