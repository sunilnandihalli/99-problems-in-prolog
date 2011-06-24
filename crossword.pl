:-ensure_loaded(p99-readfile).
bind_crossword(1,'p99a.dat').
bind_crossword(2,'p99b.dat').
bind_crossword(3,'p99c.dat').
bind_crossword(4,'p99d.dat').

is_word([X]):-!,char_type(X,alpha).
is_word([X|Xs]):-
	char_type(X,alpha),
	is_word(Xs).

generate_word_list(L,WL):-
	setof(WordLength-Word,
	      (member(Word,L),is_word(Word),length(Word,WordLength)),
	      WL).

is_puzzle_entry('.').
is_puzzle_entry(' ').

is_puzzle_list(S):-
	maplist(is_puzzle_entry,S),
	\+ S=[].

generate_syms([],[]).
generate_syms(['.'|Xs],[_|Syms]):-!,
	generate_syms(Xs,Syms).
generate_syms([' '|Xs],[' '|Syms]):-!,
	generate_syms(Xs,Syms).
generate_puzzle(L,P):-
	bagof(ML,
	      (member(ML,L),is_puzzle_list(ML)),
	      PL),
	maplist(generate_syms,PL,P).

is_valid_puzzle_entry(X):-var(X),!.
is_valid_puzzle_entry(' ').


chk_prow(PR):-
	maplist(is_valid_puzzle_entry,PR).

chk_assum(P):-
	maplist(chk_prow,P).


is_valid_word_entry(X):-var(X),!.
is_valid_word_entry(X):-char_type(X,upper).

get_word([],[],[]).
get_word([X|Xs],[X|RestOfWord],RemainingInputList):-
	is_valid_word_entry(X),!,
	get_word(Xs,RestOfWord,RemainingInputList).
get_word(RemaingInputList,[],RemaingInputList).

is_valid_word([_W1,_W2|_Ws]).

words_list([],[]):-!.
words_list([X|Xs],L):-
	\+ is_valid_word_entry(X),!,
	words_list(Xs,L).
words_list(R,[W|Ws]):-
	get_word(R,W,Rs),
	is_valid_word(W),!,
	words_list(Rs,Ws).
words_list(R,Ws):-
	get_word(R,_,Rs),!,
	words_list(Rs,Ws).

getwords([],[]).
getwords([R|Rs],L):-
	words_list(R,L1),
	getwords(Rs,L2),
	append(L1,L2,L).

get_col([],[],[]).
get_col([[X|Xs]|Rs],[X|Cs],[Xs|RXs]):-
	get_col(Rs,Cs,RXs).

get_cols([[]|_],[]).
get_cols(Rs,[C|Cs]):-
	get_col(Rs,C,RRs),
	get_cols(RRs,Cs).
	
get_puzzle_words(P,Ws):-
	getwords(P,Ws1),
	get_cols(P,PC),
	getwords(PC,Ws2),
	append(Ws1,Ws2,Ws).

add_length_prefix(V,N-V):-
	length(V,N).
tag_permutation_groups_with_length([],[]).
tag_permutation_groups_with_length([_-[X,Y]|Gs],[K-[X,Y]|PGs]):-
	length(X,K),
	tag_permutation_groups_with_length(Gs,PGs).

unify_list([],[]).
unify_list([X|Xs],[X|Ys]):-
	unify_list(Xs,Ys).

unify_permutable_group([],[]).
unify_permutable_group([G1|G1s],G2L):-
	select(G2,G2L,G2s),
	unify_list(G1,G2),
	unify_permutable_group(G1s,G2s).

unify_permutable_groups([]).
unify_permutable_groups([_-[G1,G2]|Gs]):-
	unify_permutable_group(G1,G2),
	unify_permutable_groups(Gs).
		       


print_crossie_row(L):-
	maplist(write,L),nl.

testc(N):-
	bind_crossword(N,FName),
	read_lines(FName,L),
	generate_word_list(L,WL),
	generate_puzzle(L,P),
	get_puzzle_words(P,Ws),
	maplist(add_length_prefix,Ws,Wsp),
	msort(Wsp,Wsp1),
	group_pairs_by_key(Wsp1,GWsp),
	group_pairs_by_key(WL,GWL),
	append(GWsp,GWL,G),
	msort(G,G1),
	group_pairs_by_key(G1,GG),
	tag_permutation_groups_with_length(GG,GG1),
	msort(GG1,GG2),
	unify_permutable_groups(GG2),
	maplist(print_crossie_row,P).

	
	