decode(X,Y):-runlengthdecode(X,Z),append(Z,Y).
runlengthdecode([],[]).
runlengthdecode([C|Cs],[D|Ds]):-decodeelement(C,D),runlengthdecode(Cs,Ds).
decodeelement([1,X],[X]).
decodeelement([N,X],[X|Ds]):- N1 is N-1,decodeelement([N1,X],Ds).
decodeelement(X,[X]).