bind(true).
bind(fail).
unwind(_):-bind(A),write(A),A,!,write(' here '),\+ A.