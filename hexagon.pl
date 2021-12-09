% ##################################################################################
% --------------------------------------HEXAGON--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(hexagon,
    [
        cardinal_direction_to_axial/3,
        axial_neighbors/2,
        are_neighbors/2,
        anti_neighborhood/3,
        get_QR/3,
        is_a_hinged_hex/2
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(utils), import(utils).

% --------------------------------------DYNAMICS--------------------------------------
% ...

% --------------------------------------Cardinal Direction--------------------------------------
% cardinal_direction_to_axial(NumericDir, CardinalDir, Hex).
cardinal_direction_to_axial(1, nw, [0, -1]).
cardinal_direction_to_axial(2, w, [-1, 0]).
cardinal_direction_to_axial(3, sw, [-1, 1]).
cardinal_direction_to_axial(4, se, [0, 1]).
cardinal_direction_to_axial(5, e, [1, 0]).
cardinal_direction_to_axial(6, ne, [1, -1]).

% --------------------------------------METHODS--------------------------------------

% --------------------------------------Coordinate conversion--------------------------------------
% cube_to_axial(Cube, Hex)
cube_to_axial([Q, R, _], Hex):-
    number(Q),
    number(R),
    var(Hex),
    Hex = [Q, R].

% axial_to_cube(Hex, Cube)
axial_to_cube([Q, R], Cube):-
    number(Q),
    number(R),
    var(Cube),
    S is -Q-R,
    Cube = [Q, R, S].

% axial_to_oddr(Hex, OffsetCoord)
axial_to_oddr([Q, R], OffsetCoord):-
    number(Q),
    number(R),
    var(OffsetCoord),
    Col is Q + (R - (R mod 2)) / 2,
    Row = R,
    OffsetCoord = [Col, Row].

% oddr_to_axial(OffsetCoord, Hex)
oddr_to_axial([Col, Row], Hex):-
    number(Col),
    number(Row),
    var(Hex),
    Q = Col - (Row - (Row mod 2)) / 2,
    R = Row,
    Hex = [Q, R].

% --------------------------------------Neighbors--------------------------------------
% axial_direction_vectors(Adv)
axial_direction_vectors(Axial_direction_vectors):-
    findall(Hex, cardinal_direction_to_axial(_, _, Hex), Axial_direction_vectors).

% axial_direction(Dir, Axial_direction_vector)
axial_direction(Dir, Vec):-
    axial_direction_vectors(Axial_direction_vectors),
    utils:element_at(Vec, Axial_direction_vectors, Dir).

% axial_add(Hex1, Vec, Hex2)
axial_add([Q1, R1], [Q2, R2], Hex):-
    Q3 is Q1 + Q2,
    R3 is R1 + R2,
    Hex = [Q3, R3].

% axial_neighbor(Hex, Dir, Neighbor)
axial_neighbor(Hex, Dir, Neighbor):-
    axial_direction(Dir, Vec),
    axial_add(Hex, Vec, Neighbor).

% axial_neighbors(Hex, Neighbors)
axial_neighbors(Hex, Neighbors):-
    findall(Neighbor, axial_neighbor(Hex, _, Neighbor), Neighbors).

% true if Hex1 and Hex2 are neighbors.
are_neighbors(Hex1, Hex2):-
    axial_neighbors(Hex1, N),
    member(Hex2, N),!.

% elements of the first list that are not neighbors of any element of the second list
% anti_neighborhood(L1, L2, L3)
anti_neighborhood(L1, L2, L3):-
    findall(X, (member(X, L1),not(be_a_neighbor(X, L2))), L3).

% if Hex is a neighbor of any hexagon in Hexs
be_a_neighbor(Hex, Hexs):-
    member(H, Hexs),
    are_neighbors(Hex, H),!.

% get_QR(Hexagon)
% returns the q and r of a hexagon
get_QR([Q,R], Q, R).

% two hexagons are adjacent if they are on the board and are neighbors
adj(H1,H2, L):-
    member(H1, L),
    member(H2, L),
    are_neighbors(H1,H2).

% DFS | We all know what it does :D
dfs_visit([[Goal|Path]|_],Goal,[Goal|Path],0,_).
dfs_visit([Path|Queue],Goal,FinalPath,N,Hive) :-
    extend(Path,NewPaths, Hive),
    append(NewPaths,Queue,NewQueue),
    dfs_visit(NewQueue,Goal,FinalPath,M,Hive),
    N is M+1.

% extend the path without cycles
extend([Node|Path],NewPaths, Hive) :-
    findall([NewNode,Node|Path],
            (adj(Node,NewNode,Hive), 
            \+ member(NewNode,Path)), % for avoiding loops
            NewPaths).


% tells if there is a path in the graph that joins the two hexagons (if they are connected)
is_connected(Hex1,Hex2,Hive):-
    dfs_visit([[Hex1]],Hex2,_,_,Hive),!.

% returns list with neighbors placed in the hive
axial_neighbors_in_hive(Hex,Hive,L):-
    axial_neighbors(Hex, Neighbors),
    findall(X,(member(X,Neighbors),member(X,Hive)), L).

% a hexagon is hinged if it is a hinge node in the graph formed by all hexagons...
% I remove the candidate hexagon to point of articulation 
% and analyze if all its neighbors are still connected
is_a_hinged_hex(Hex,Hive):-
    axial_neighbors_in_hive(Hex,Hive,L),
    member(H1,L),
    member(H2,L),
    H1 \= H2,
    H1 \= Hex,
    H2 \= Hex,
    utils:delete(Hex,Hive,Hive1),
    not(is_connected(H1,H2,Hive1)),
    !.

