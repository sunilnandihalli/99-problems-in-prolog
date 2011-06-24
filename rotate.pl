:-ensure_loaded(splitn).
myrotate(L,N,Lr):-split(L,N,L1,L2),append([L2,L1],Lr).