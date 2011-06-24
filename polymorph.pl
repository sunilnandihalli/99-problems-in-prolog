bind_hello(sunil(a)).
bind_hello(victor(b)).
bind_hello(satish(c)).
hello(sunil(X),satish([X,X])).
hello(sunil(X),victor([X,X,X])).
hello(victor(X),satish([X,X,X,X])).
