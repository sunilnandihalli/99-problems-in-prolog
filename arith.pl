init:-
	add_db(paren,['(',')']),
	add_db(operator,['+','-','*','/','=']),
	add_db(operator_ne,['+','-','*','/']).

add_db(_,[]).
add_db(X,[V|Vs]):-
	recorda(X,V),
	add_db(X,Vs).

combination_without_paranthesis([N],[N]):-!.
combination_without_paranthesis([N|Ns],[N,Op|Cs]):-
	combination_without_paranthesis(Ns,Cs),
	recorded(operator,Op),
	\+ (memberchk('=',Cs),Op='=').

add_parenthesis(FVC_NP,FVC_NP).

accept(L):-
	append([First,['='],Second],L),!,
	evaluate(First,LVal),
	evaluate(Second,RVal),
	LVal=RVal.

evaluate(L,S):-
	append([First,['+'],Second],L),!,
	evaluate(First,S1),
	evaluate(Second,S2),
	S is S1+S2.

evaluate(L,S):-
	append([First,['-'],Second],L),!,
	evaluate(First,S1),
	evaluate(Second,S2),
	S is S1-S2.

evaluate(L,S):-
	append([First,['*'],Second],L),!,
	evaluate(First,S1),
	evaluate(Second,S2),
	S is S1*S2.

evaluate(L,S):-
	append([First,['/'],Second],L),!,
	evaluate(First,S1),
	evaluate(Second,S2),
	\+ S2 = 0,
	S is S1/S2.

evaluate(L,S):-
	append(['(',L1,')'],L),!,
	evaluate(L1,S).


evaluate([L],L).
		     


		 
find_valid_combination(L,FVC):-
	combination_without_paranthesis(L,FVC_NP),
	memberchk('=',FVC_NP),
	add_parenthesis(FVC_NP,FVC).

solve(L,FVC):-
	find_valid_combination(L,FVC),
	accept(FVC).

data_type(paranthesis).
data_type(operator).

clear_db(X):-
	recorded(X,_,Ref),
	erase(Ref),
	fail.
clear_db(_).


do(L):-
	clear_db(paren),
	clear_db(operator),
	clear_db(operator_ne),
	init,
	solve(L,S),
	write(S),nl,
	fail.

test(1):-
	do([2,3,5,7,11]).

test(2):-
	do([1,2,3]).

teval(1):-
	accept([1,'+',2,'=',4]).

teval1(2,L):-
	evaluate([1,'+',2],L).

