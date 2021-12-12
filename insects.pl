:- module(insects, 
    [
        insect/6,
        placeInsect/4,
        allInsects/7,
        initInsects/0,
        placementsAvailales/3,
        movesAvailables/6,
        moveInsect/7,
        canPlay/2,
        isQueenSurrounded/1,
        canPlaceAny/3
    ]).

:-use_module("./geometry/hexagonal").
:-use_module("./utils/list_utils").
:-use_module("./utils/hex_utils").

:-dynamic insect/6, box/1, node/2.

swapPlayer(p1, p2).
swapPlayer(p2 ,p1).

canPlaceAny(Player_id, Number_of_moves, Placements):-
    placementsAvailales(Player_id, Number_of_moves, Placements),
    Placements \= [],!.

canMoveAny(_, _):-
    !.

canPlay(Player_id, Number_of_moves):-
    not(canPlaceAny(Player_id, Number_of_moves, _)),
    not(canMoveAny(Player_id, Number_of_moves)),
    fail,!.
canPlay(_, _, _):-!.

isQueenSurrounded(Player_id):-
    insect(queen_bee, _, Player_id, Hex, true, 0),
    neighborsCount(Hex, Len),
    Len == 6.

isAPoint(Hex):-
    getLvlForPos(Hex, Lvl),
    Lvl > 0,
    !,
    fail.
isAPoint(Hex):-
    getPlacedNeighbors(Hex, PNs),
    [PN|_]=PNs,
    allInsects(_, _, _, _, true, 0, Insects),
    length(Insects, Len_insects0),
    Len_insects is Len_insects0 - 1,
    current_prolog_flag(max_tagged_integer, MaxI),
    bfs([[PN, 0]], [Hex], MaxI, isAdj3),!,
    findall(Node, (node(Node, Lvl), Lvl > -1), Nodes),
    retractall(node(_, _)),
    length(Nodes, Len_nodes),
    Len_nodes \= Len_insects,
    !.
isAPoint(_):-fail.


placementsAvailales(Player_id, Number_of_moves, Placements):-
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p1,
    Number_of_moves == 0,
    Placements = [[0, 0]],
    !.
placementsAvailales(Player_id, Number_of_moves, Placements):-
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p2,
    Number_of_moves == 0,
    hexagon:axial_neighbors([0, 0], Placements),
    !.
placementsAvailales(Player_id, Number_of_moves, Placements):-
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    swapPlayer(Player_id, Other_player_id),
    getNeighborsOfAll(Player_id, Void_neighbors),
    findall(Hex_other_player, insect(_, _, Other_player_id, Hex_other_player, true, _), Hexagons),
    hexagon:anti_neighborhood(Void_neighbors, Hexagons, Placements).

movesAvailables(Player_id, _, _, _, MSG, Status_Code):-
    not(insect(queen_bee, _, Player_id, _, true, 0)),
    MSG = "Add your QUEEN if you want to move!",
    Status_Code = 400,
    !.
movesAvailables(Player_id, Type, Id, Hexagon, MSG, Status_Code):-
    insect(Type, Id, Player_id, Hexagon, true, _),
    isBlocked(Type,Id,Player_id,Hexagon),
    MSG = "Insect blocked!",
    Status_Code = 400,
    !.
movesAvailables(Player_id, Type, Id, Hexagon, MSG, Status_Code):-
    insect(Type, Id, Player_id, Hexagon, true, _),
    isAPoint(Hexagon),
    MSG = "Cannot move because this would break the hive in 2.",
    Status_Code = 400,
    !.
movesAvailables(Player_id, Type, _, Hexagon, Moves, Status_Code):-
    switch(Type,
        [
            queen_bee: isQueenAvailableToMove(Player_id, Hexagon, Moves, Status_Code),
            beetle: beetleAvailableMoves(Player_id, Hexagon, Moves, Status_Code),
            grasshopper: grasshopperAvailableMoves(Player_id, Hexagon, Moves, Status_Code),
            spider: spiderAvailableMoves(Player_id, Hexagon, Moves, Status_Code),
            soldier_ant: antPossibleMoves(Player_id, Hexagon, Moves, Status_Code)
        ]).

switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

getAvailableMovesInDir(_, Hex, PM, N):-
    N == 0,
    notHasInsect(Hex),
    PM = [],!.
getAvailableMovesInDir(Dir, Hex, PM, N):-
    N == 0,
    not(notHasInsect(Hex)),
    hexagon:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    getAvailableMovesInDir(Dir, NHex, PM, N1).

getAvailableMovesInDir(_, Hex, PM, N):-
    N > 0,
    notHasInsect(Hex),
    PM = Hex,!.
getAvailableMovesInDir(Dir, Hex, PM, N):-
    N > 0,
    not(notHasInsect(Hex)),
    hexagon:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    getAvailableMovesInDir(Dir, NHex, PM, N1).

isQueenAvailableToMove(Player_id, Hexagon, MSG, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true, 0),
    getNeighborsV2( Hexagon, VN),
    findall(X, 
        (
            member(X, VN),
            isMoveValid(Hexagon, X)
        ),L),
        L == [],
        MSG = 'Queen bee has no allowed destination.',
        Status_Code = 400,!.
isQueenAvailableToMove(Player_id, Hexagon, Moves, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true, 0),
    getNeighborsV2(Hexagon, VN),
    findall(X, 
        (
            member(X, VN),
            isMoveValid(Hexagon, X)
        ),Moves),
    Status_Code = 200.

beetleAvailableMoves(Player_id, Hexagon, MSG, Status_Code):-
    getLastInsect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    getNeighbors(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            isMoveValid(X, Hexagon)
        ),L1),
    getPlacedNeighbors(Hexagon, L2),
    append(L1, L2, Moves),
    Moves == [],
    MSG = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetleAvailableMoves(Player_id, Hexagon, MSG, Status_Code):-
    getLastInsect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    getNeighbors(Player_id, Hexagon, VN),
    getPlacedNeighbors(Hexagon, PN),
    append(VN, PN, Moves),
    Moves == [],
    MSG = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetleAvailableMoves(Player_id, Hexagon, Moves, Status_Code):-
    getLastInsect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    getNeighbors(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            isMoveValid(Hexagon, X)
        ),L1),
    getPlacedNeighbors(Hexagon, L2),
    append(L1, L2, Moves),
    Status_Code = 200,!.
beetleAvailableMoves(Player_id, Hexagon, Moves, Status_Code):-
    getLastInsect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    getNeighbors(Player_id, Hexagon, VN),
    getPlacedNeighbors(Hexagon, PN),
    append(VN, PN, Moves),
    Status_Code = 200,!.

grasshopperAvailableMoves(Player_id, Hexagon, MSG, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagon, true, 0),
    hexagon:axial_neighbor(Hexagon, 1, NW),
    hexagon:axial_neighbor(Hexagon, 2, W),
    hexagon:axial_neighbor(Hexagon, 3, SW),
    hexagon:axial_neighbor(Hexagon, 4, SE),
    hexagon:axial_neighbor(Hexagon, 5, E),
    hexagon:axial_neighbor(Hexagon, 6, NE),
    
    getAvailableMovesInDir(1, NW, PM1, 0),
    getAvailableMovesInDir(2, W, PM2, 0),
    getAvailableMovesInDir(3, SW, PM3, 0),
    getAvailableMovesInDir(4, SE, PM4, 0),
    getAvailableMovesInDir(5, E, PM5, 0),
    getAvailableMovesInDir(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6] ,[], L),
    list_utils:delete_all_occurrences([], L ,L1),
    L1 == [],
    MSG = 'Grasshopper bee has no allowed destination.',
    Status_Code = 400,!.
grasshopperAvailableMoves(Player_id, Hexagon, Moves, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagon, true, 0),
    hexagon:axial_neighbor(Hexagon, 1, NW),
    hexagon:axial_neighbor(Hexagon, 2, W),
    hexagon:axial_neighbor(Hexagon, 3, SW),
    hexagon:axial_neighbor(Hexagon, 4, SE),
    hexagon:axial_neighbor(Hexagon, 5, E),
    hexagon:axial_neighbor(Hexagon, 6, NE),
    
    getAvailableMovesInDir(1, NW, PM1, 0),
    getAvailableMovesInDir(2, W, PM2, 0),
    getAvailableMovesInDir(3, SW, PM3, 0),
    getAvailableMovesInDir(4, SE, PM4, 0),
    getAvailableMovesInDir(5, E, PM5, 0),
    getAvailableMovesInDir(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6],[], L),
    list_utils:delete_all_occurrences([],L ,Moves),
    Status_Code = 200.

spiderAvailableMoves(_, Hexagon, Moves, Status_Code):-
    bfs([[Hexagon, 0]], [], 3, isAdj1),!,
    findall(U, (node(U, Lvl), Lvl > -1), Moves0),
    retractall(node(_, _)),
    getNeighborsV2(Hexagon, VN),
    findall(X, (member(X, VN), not(isMoveValid(Hexagon, X))), L),
    list_utils:delete2(L, Moves0, Moves1),
    cleanspiderAvailableMoves(Hexagon, Moves1, Moves),
    Status_Code = 200.

cleanspiderAvailableMoves(Hex, L1, L2):-
    findall(Path, (
        member(X, L1), 
        hexagon:dfs_visit([[Hex]], X, Path, _, L1), 
        length(Path, Len), 
        Len == 4, 
        isSpiderPathValid(Path)
        ), Paths),
    gfb(),
    findall(H, member([H|_], Paths), L2).

isSpiderPathValid(Path):-
    reverse(Path, RPath),
    list_utils:element_at(X1, RPath, 1),!,
    list_utils:element_at(X2, RPath, 2),!,
    list_utils:element_at(X3, RPath, 3),!,
    list_utils:element_at(X4, RPath, 4),!,
    isMoveValid(X1, X2),
    isMoveValidV2(X1, X2, X3),
    isMoveValidV2(X1, X3, X4).


gfb():-
    not(box(_)),!.
gfb():-
    box(L),
    retract(box(L)),
    T=..L,
    assert(T),!.

mtb(L):-
    T=..L,
    retract(T),
    L1 = [box, L],
    T1=..L1,
    assert(T1),!.

antPossibleMoves(Player_id, Hexagon, Moves, Status_Code):-
    insect(soldier_ant, _, Player_id, Hexagon, true, _),
    current_prolog_flag(max_tagged_integer, MaxI),
    bfs([[Hexagon, 0]], [], MaxI, isAdj1),!,
    findall(U, (node(U, Lvl), Lvl > 0), Moves0),
    retractall(node(_, _)),
    getNeighborsV2(Hexagon, VN),
    findall(X, (member(X, VN), not(isMoveValid(Hexagon, X))), L),
    list_utils:delete2(L, Moves0, Moves),
    Status_Code = 200.

bfs([], _, _, _):-!.
bfs([[_, _lvl]|_], _, Lvl, _):-
    _lvl > Lvl,!.
bfs([[U, _]|Q], Visited, Lvl, AdjPred):-
    member(U, Visited),
    bfs(Q, Visited, Lvl, AdjPred).
bfs([[U, _lvl]|Q], Visited, Lvl, AdjPred):-
    not(member(U, Visited)),
    assert(node(U, _lvl)),
    Pred =..[AdjPred, [U, _lvl], A],
    call(Pred),
    % valid_adj([U, _lvl], A),
    append([U], Visited, Visited1),
    append(Q, A, Q1),
    bfs(Q1, Visited1, Lvl, AdjPred).

isAdj1([U, Lvl], A):-
    Lvl1 is Lvl + 1,
    getNeighborsV2(U, N),

    findall([V, Lvl1], 
        (
            member(V,N),
            not(isRoadBlocked(U, V)),
            hasOneNeighborInArena(V)
        ), A).

isAdj2([U, Lvl], A):-
    Lvl1 is Lvl + 1,
    getNeighborsV2(U, N),

    findall([V, Lvl1], 
        (
            member(V,N)
        ), A).

isAdj3([U, Lvl], A):-
    Lvl1 is Lvl + 1,
    getPlacedNeighbors(U, N),

    findall([V, Lvl1], 
        (
            member(V,N)
        ), A).

ladybugAvailableMoves(Player_id, Hexagon, Moves, Status_Code):-
    insect(ladybug, _, Player_id, Hexagon, true,_),
    getNeighbors(Player_id, Hexagon, Moves),
    Status_Code = 200.

getNeighbors(Player_id, Hex, Void_neighbors):-
    swapPlayer(Player_id, Other_player_id),
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H1, insect(_, _, Player_id, H1, true,_), Hexagons1),
    findall(H2, insect(_, _, Other_player_id, H2, true,_), Hexagons2),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons1)), not(member(X, Hexagons2))), Void_neighbors).

getNeighborsV2(Hex, Void_neighbors):-
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true,_), Hexagons),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons))), Void_neighbors).

getPlacedNeighbors(Hex, Placed_neighbors):-
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true, 0), Hexagons),
    findall(X, (member(X, Neighbors), member(X, Hexagons)), Placed_neighbors).

commonNeighborsCount(H1, H2, Len):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X,Set), not(notHasInsect(X))),L),
    length(L,Len).
commonNeighborsCountV2(Hexagon, H1, H2, Len):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X, Set), not(member(Hexagon, Set)), not(notHasInsect(X))),L),
    length(L,Len).

neighborsCount(Hex, A):-
    getPlacedNeighbors(Hex, PN),
    length(PN, A),!.

isRoadBlocked(H1, H2):-
    commonNeighborsCount(H1, H2, Len),
    Len == 2.

isMoveValid(H1, H2):-
    commonNeighborsCount(H1, H2, Len),
    Len == 1.
isMoveValidV2(Hexagon, H1, H2):-
    commonNeighborsCountV2(Hexagon, H1, H2, Len),
    Len == 1.

hasOneNeighborInArena(U):-
    getPlacedNeighbors(U, PN),
    length(PN, Len),
    Len > 0.

getHexagons(Hive_hex):-
    findall(H, insect(_, _, _, H, true,_), Hive_hex).

getNeighborsOfAll(Player_id, Void_neighbors):-
    findall(VN, (insect(_, _, Player_id, Hex, true,_), getNeighbors(Player_id, Hex, VN)), Void_neighbors_aux1),
    hex_utils:flatten_hex(Void_neighbors_aux1, Void_neighbors_aux2),
    setof(X, member(X, Void_neighbors_aux2), Void_neighbors).
get_void_neighbors_of_all_hex_2(Void_neighbors):-
    findall(VN, (insect(_, _, _, Hex, true,_), getNeighborsV2(Hex, VN)), Void_neighbors_aux),
    hex_utils:flatten_hex(Void_neighbors_aux, Void_neighbors_aux1),
    setof(X, member(X, Void_neighbors_aux1), Void_neighbors).

placeInsect(Player_id, Type, Hex, Insect):-
    var(Insect),
    atom(Player_id),
    atom(Type),
    compound(Hex),
    var(Insect),
    insect(Type, Id, Player_id, none, false,-1),
    !,
    retract(insect(Type, Id, Player_id, none, false,-1)),
    assert(insect(Type, Id, Player_id, Hex, true,0)),
    insect(Type, Id, Player_id, Hex, true,0)=..Insect.

moveInsect(Type, Id, Player_id, Lvl, Hexagon_Ori, Hexagon_End, InsectRes):-
    retract(insect(Type, Id, Player_id, Hexagon_Ori, true, Lvl)),

    getLvlForPos(Hexagon_End, Lvl_end),

    Lvl1 is Lvl_end + 1,

    assert(insect(Type, Id, Player_id, Hexagon_End, true, Lvl1)),
    InsectRes = [Type, Id, Player_id, Hexagon_End, true, Lvl1],!.


allInsects(Type, Id, Player_id, Hex, Placed, Lvl, Insects):-
    findall([Type, Id, Player_id, Hex, Placed, Lvl], insect(Type, Id, Player_id, Hex, Placed, Lvl), Insects).
initInsects():-
    assert(insect(queen_bee, 1, p1, none, false, -1)),
    
    assert(insect(queen_bee, 1, p2, none, false, -1)),

    assert(insect(beetle, 1, p1, none, false, -1)),
    assert(insect(beetle, 2, p1, none, false, -1)),
    assert(insect(beetle, 1, p2, none, false, -1)),
    assert(insect(beetle, 2, p2, none, false, -1)),

    assert(insect(grasshopper, 1, p1, none, false, -1)),
    assert(insect(grasshopper, 2, p1, none, false, -1)),
    assert(insect(grasshopper, 3, p1, none, false, -1)),
    assert(insect(grasshopper, 1, p2, none, false, -1)),
    assert(insect(grasshopper, 2, p2, none, false, -1)),
    assert(insect(grasshopper, 3, p2, none, false, -1)),

    assert(insect(spider, 1, p1, none, false, -1)),
    assert(insect(spider, 2, p1, none, false, -1)),
    assert(insect(spider, 1, p2, none, false, -1)),
    assert(insect(spider, 2, p2, none, false, -1)),

    assert(insect(soldier_ant, 1, p1, none, false, -1)),
    assert(insect(soldier_ant, 2, p1, none, false, -1)),
    assert(insect(soldier_ant, 3, p1, none, false, -1)),
    assert(insect(soldier_ant, 1, p2, none, false, -1)),
    assert(insect(soldier_ant, 2, p2, none, false, -1)),
    assert(insect(soldier_ant, 3, p2, none, false, -1)),

    assert(insect(ladybug, 1, p1, none, false, -1)),
    assert(insect(ladybug, 1, p2, none, false, -1)),

    assert(insect(mosquito, 1, p1, none, false, -1)),
    assert(insect(mosquito, 1, p2, none, false, -1)),

    assert(insect(pillbug, 1, p1, none, false, -1)),
    assert(insect(pillbug, 1, p2, none, false, -1)).

notHasInsect(Hex):-
    not(insect(_, _, _, Hex, true,_)),!.

isBlocked(Type,Id,Pid,Hex):-
    insect(Type, Id, Pid, Hex, true, Lvl),
    Lvl1 is Lvl+1,
    insect(_, _, _, Hex, true, Lvl1),!.

getLvlForPos(Hex, Lvl):-
    notHasInsect(Hex),
    Lvl is -1,!.
getLvlForPos(Hex, Lvl):-
    findall(Lvl1, insect(_,_,_,Hex,true,Lvl1), L),
    list_utils:last_element(Lvl,L).

getLastInsect(Hex, I):-
    notHasInsect(Hex),
    I=[],!.
getLastInsect(Hex, I):-
    getLvlForPos(Hex, Lvl),
    insect(Type, Id, Pid, Hex, true, Lvl),
    I = [Type, Id, Pid, Hex, true, Lvl].
