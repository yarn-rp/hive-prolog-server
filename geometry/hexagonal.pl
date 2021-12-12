:- module(hexagon,
    [
        cardinal_direction_to_axial/3,
        axial_neighbors/2,
        are_neighbors/2,
        anti_neighborhood/3,
        get_QR/3
    ]).

:-use_module("../utils/list_utils").

cardinal_direction_to_axial(1, nw, [0, -1]).
cardinal_direction_to_axial(2, w, [-1, 0]).
cardinal_direction_to_axial(3, sw, [-1, 1]).
cardinal_direction_to_axial(4, se, [0, 1]).
cardinal_direction_to_axial(5, e, [1, 0]).
cardinal_direction_to_axial(6, ne, [1, -1]).

axial_direction_vectors(Axial_direction_vectors):-
    findall(Hex, cardinal_direction_to_axial(_, _, Hex), Axial_direction_vectors).

axial_direction(Dir, Vec):-
    axial_direction_vectors(Axial_direction_vectors),
    list_utils:element_at(Vec, Axial_direction_vectors, Dir).

axial_add([Q1, R1], [Q2, R2], Hex):-
    Q3 is Q1 + Q2,
    R3 is R1 + R2,
    Hex = [Q3, R3].

axial_neighbor(Hex, Dir, Neighbor):-
    axial_direction(Dir, Vec),
    axial_add(Hex, Vec, Neighbor).

axial_neighbors(Hex, Neighbors):-
    findall(Neighbor, axial_neighbor(Hex, _, Neighbor), Neighbors).

are_neighbors(Hex1, Hex2):-
    axial_neighbors(Hex1, N),
    member(Hex2, N),!.

anti_neighborhood(L1, L2, L3):-
    findall(X, (member(X, L1),not(be_a_neighbor(X, L2))), L3).

be_a_neighbor(Hex, Hexs):-
    member(H, Hexs),
    are_neighbors(Hex, H),!.

get_QR([Q,R], Q, R).

adj(H1,H2, L):-
    member(H1, L),
    member(H2, L),
    are_neighbors(H1,H2).

dfs_visit([[Goal|Path]|_],Goal,[Goal|Path],0,_).
dfs_visit([Path|Queue],Goal,FinalPath,N,Hive) :-
    extend(Path,NewPaths, Hive),
    append(NewPaths,Queue,NewQueue),
    dfs_visit(NewQueue,Goal,FinalPath,M,Hive),
    N is M+1.

extend([Node|Path],NewPaths, Hive) :-
    findall([NewNode,Node|Path],
            (adj(Node,NewNode,Hive), 
            \+ member(NewNode,Path)),
            NewPaths).


is_connected(Hex1,Hex2,Hive):-
    dfs_visit([[Hex1]],Hex2,_,_,Hive),!.

axial_neighbors_in_hive(Hex,Hive,L):-
    axial_neighbors(Hex, Neighbors),
    findall(X,(member(X,Neighbors),member(X,Hive)), L).
