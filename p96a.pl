% P96 (**) Syntax checker for Ada identifiers (Difference lists)

% A purely recursive syntax is:
%
% <identifier> ::= <letter> <rest>
%
% <rest> ::=  | <optional_underscore> <letter_or_digit> <rest>
%
% <optional_underscore> ::=  | '_'
%
% <letter_or_digit> ::= <letter> | <digit>

% identifier(Str) :- Str is a legal Ada identifier
%    (atom) (+)

identifier(S) :- atom(S), atom_chars(S,L), identifier(L-[]).

identifier([X|L]-R) :- char_type(X,alpha), rest(L-R).

rest(T-T) :- !.
rest(L-R) :- 
   optional_underscore(L-L1),
   letter_or_digit(L1-L2),
   rest(L2-R).

optional_underscore(['_'|R]-R) :- !.
optional_underscore(T-T).

letter_or_digit([X|R]-R) :- char_type(X,alpha), !.
letter_or_digit([X|R]-R) :- char_type(X,digit).

% This solution with difference lists is not more elegant as the 
% simple solution with ordinary lists (see p96.pl), because the
% parsing is done by always removing just one or two characters
% from the head of the list. Take this as an easy exercise!
