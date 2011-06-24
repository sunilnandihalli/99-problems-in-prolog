bind_char_list("abc(,des)").
bind_char_list("abd(,)").
bind_char_list("a").
bind_char_list("abc").
bind_char_list("abc(ded,feg)").
bind_char_list("a(b,c)").
bind_char_list("a(b,c),d(,e)").
bind_char_list("a(b(c,d),e(f,g))").
bind_char_list("ab(,cd(,de))").
bind_char_list("a(b(d,e),c(,f(g,)))").
bind_char_list("a ( b ( d , e ),c(,f(gfs,)))").
bind_char_list("a ( b ( d, e),c(,f(g f,)))").
bind_char_list("a ( b ( d;s, e;),c(,f(gf,)))").
bind_char_list("a(,b,s)").
bind_char_list("a(c,d,e,g)").
bind_char_list(":a(d,c)").
bind_char_list("a(d,,c)").
bind_string(S):-bind_char_list(L),string_to_list(S,L).
bind_tree(t(a,t(b,nil,nil),t(cd,nil,nil))).
bind_tree(t(abc,t(ade,nil,nil),nil)).

simplify_trivial_node(t(nil,nil,nil),nil):-!.
simplify_trivial_node(X,X).

string_to_node(t(Atom,nil,nil),NodeString):-
	my_string_to_atom(NodeString,Atom).

string_to_node(t(Atom,LeftNode,RightNode),NodeString):-
	append([AtomString,"(",NodeString1,",",NodeString2,")"],NodeString),
	string_to_node(LeftNode1,NodeString1),
	simplify_trivial_node(LeftNode1,LeftNode),
	string_to_node(RightNode1,NodeString2),
	simplify_trivial_node(RightNode1,RightNode),
	my_string_to_atom(AtomString,Atom).

tree_list(Tree,ListOfChars):-
	var(Tree),
	nonvar(ListOfChars),
	string_to_node(ListOfChars,Tree).

tree_list(Tree,String):-
	nonvar(Tree),
	var(ListOfChars),
	node_to_string(Tree,ListOfChars),
	string_to_list(String,ListOfChars).
node_to_string(nil,[]).
node_to_string(t(Atom,LeftNode,RightNode),NodeString):-
	atom_chars(Atom,AtomString),
	node_to_string(LeftNode,LeftNodeString),
	node_to_string(RightNode,RightNodeString),
	append([AtomString,['('],LeftNodeString,[','],RightNodeString,[')']],
	       NodeString).

remove_space(S1,S2):-
	remove_leading_space(S1,S3),
	remove_trailing_space(S3,S2).

remove_leading_space([],[]):-!.
remove_leading_space([X|Xs],Xs1):-
	char_type(X,white),
	remove_leading_space(Xs,Xs1),!.
remove_leading_space(S,S).

remove_trailing_space(S1,S2):-
	reverse(S1,S3),
	remove_leading_space(S3,S4),
	reverse(S4,S2).

is_alphabet(X):-
	char_type(X,alpha).

my_string_to_atom([],nil):-!.
my_string_to_atom(AtomString,Atom):-
	remove_space(AtomString,AtomStringWithOutSpace),
	maplist(is_alphabet,AtomStringWithOutSpace),
	atom_chars(Atom,AtomStringWithOutSpace).


