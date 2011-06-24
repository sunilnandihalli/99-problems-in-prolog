% (**) P87a Depth-first order graph traversal

% Write a predicate that generates a depth-first order graph
% traversal sequence. The starting point should be specified,
% and the output should be a list of nodes that are reachable from
% this starting point (in depth-first order).

% The main problem is that if we traverse the graph recursively,
% we must store the encountered nodes in such a way that they
% do not disappear during the backtrack step.

% In this solution we use the "recorded database" which is a
% more efficient alternative to the well-known assert/retract 
% mechanism. See the SWI-Prolog manuals for details.

% Alternative solution using acjacency list

:- ensure_loaded(p80).  % conversions

depth_first_order(Graph,Start,Seq) :- 
   alist_gterm(_,Alist,Graph),
   clear_rdb(dfo),
   dfo(Alist,Start),
   bagof(X,recorded(dfo,X),Seq).

dfo(_,X) :- recorded(dfo,X).
dfo(Alist,X) :-
   \+ recorded(dfo,X),
   recordz(dfo,X),
   memberchk(n(X,AdjNodes),Alist),
   Pred =.. [dfo,Alist],        % see remark below
   checklist(Pred,AdjNodes).

clear_rdb(Key) :-
   recorded(Key,_,Ref), erase(Ref), fail.
clear_rdb(_).

% The construction of the predicate Pred and the use of the checklist/2
% predefined predicate may seem strange at first. It is equivalent to 
% the following construction:
%
% dfo(_,X) :- recorded(dfo,X).
% dfo(Alist,X) :-
%    \+ recorded(dfo,X),
%    recordz(dfo,X),
%    memberchk(n(X,AdjNodes),Alist),
%    dfo_list(Alist,AdjNodes).
%
% dfo_list(_,[]).
% dfo_list(Alist,[A|As]) :- dfo(Alist,A), dfo_list(Alist,As).
