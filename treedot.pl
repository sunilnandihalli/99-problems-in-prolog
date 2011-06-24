treedot(".",nil):-!.
treedot([X],t(X,nil,nil)).
treedot([X|Xs],t(X,Left,Right)):-
	append([LeftString,RightString],Xs),
	write('left : '),write(LeftString),nl,
	write('right: '),write(RightString),nl,
	treedot(LeftString,Left),
	treedot(RightString,Right).