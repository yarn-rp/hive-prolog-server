:- module(hex_utils, 
    [element_hex/2,
    flatten_hex/2
    ]).

:- use_module("./list_utils",[isList/1,concat/3]).

isHex([Q, R]):-
    number(Q),
    number(R).

element_hex(X, [X]):-
    isHex(X),!.
element_hex(X, [X]):-
    not(isList(X)).
element_hex(X, V):-
    isList(X),
    flatten_hex(X,V).

flatten_hex([],[]).
flatten_hex([X|Y],R):- 
    element_hex(X,V),
    flatten_hex(Y,L),
    concat(V,L,R).
