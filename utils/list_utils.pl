:-module(list_utils, 
[element_at/3,
concat/3,
isList/1,
delete/3,
delete2/3,
last_element/2,
delete_all_occurrences/3
]).

element_at(X,[X|_],1).
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.

concat([], X, X):-!.
concat([X|R], Y, [X|Z]):-
    concat(R, Y, Z).

isList([]).
isList([_|_]).

delete(X,[X|R],R).
delete(X,[Y|R],[Y|R1]):-delete(X,R,R1).

delete2(L1, L2, L3):-
    findall(X, (member(X,L2), not(member(X,L1))), L3).

delete_all_occurrences(_,[],[]).
delete_all_occurrences(X,[X|T],R):-
    delete_all_occurrences(X,T,R),!.
delete_all_occurrences(X,[Y|R],[Y|R1]):-
    delete_all_occurrences(X,R,R1).

last_element(X,[X|[]]):-!.
last_element(X,[_|T]):-
    last_element(X,T).
