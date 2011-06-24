preorder(nil,[]).
preorder(t(A,B,C),[A|Xs]):-
	preorder(B,Left),
	preorder(C,Right),
	append([Left,Right],Xs).

preorder_tree(nil,[]).
preorder_tree(t(X,L,R),[X|Xs]):-
	append([LeftString,RightString],Xs),
	preorder_tree(L,LeftString),
	preorder_tree(R,RightString).

inorder(nil,[]).
inorder(t(A,B,C),Result):-
	inorder(B,Left),
	inorder(C,Right),
	append([Left,[A],Right],Result).

pre_in_order(T,InOrder,PreOrder):-
	preorder_tree(T,PreOrder),
	inorder(T,InOrder).



%add_val(X,[X|Xs]-Xs).


%preorder_diff(nil,L-L).
%preorder_diff(t(A,B,C),L1-L4):-
%	add_val(A,L1-L2),
%	preorder_diff(B,L2-L3),
%	preorder_diff(C,L3-L4).