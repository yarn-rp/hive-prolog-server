% ##################################################################################
% --------------------------------------INSECT--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(insects, 
    [
        insect/6,
        place_insect/4,
        all_insects/7,
        init_insects/0,
        possible_placements/3,
        possible_moves/6,
        move_insect/7,
        can_play/3,
        queen_surrounded/1
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(hexagon), import(hexagon).

% --------------------------------------DYNAMICS--------------------------------------
:-dynamic insect/6, box/1, node/2.
% box(L). %is to save one predicate temporarily
% insect(Type, Id, PlayerId, Hex=[Q,R], Placed, Lvl)
% node(Hex, Lvl in Bfs) is used by bfs_lvl

% other player id
other_player(p1, p2).
other_player(p2 ,p1).

% --------------------------------------METHODS--------------------------------------
% can play | Analizes if any piece can be moved or placed and in case it cannot be passed the turn
can_play(Player_id, Name, Number_of_moves):-
    possible_placements(Player_id, Number_of_moves, Placements),
    Placements == [], fail,!.
% can_play(Player_id, Name, Number_of_moves):-
%     possible_placements(Player_id, Number_of_moves, Placements),
%     Placements == [], fail,!.
can_play(_, _, _):-!.

% it is fulfilled if the queen bee is sorrounded
queen_surrounded(Player_id):-
    insect(queen_bee, _, Player_id, Hex, true, 0),
    amount_neighbors(Hex, Len),
    Len == 6.


% possibles placements
% possible_placements(Player_id, Number_of_moves, Placements)
possible_placements(Player_id, Number_of_moves, Placements):-
    % the first player's first move will always play on hexagon(0,0)
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p1,
    Number_of_moves == 0,
    Placements = [[0, 0]],
    !.
possible_placements(Player_id, Number_of_moves, Placements):-
    % the second player's first move will always play on an neighbor hexagon of hexagon(0,0)
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p2,
    Number_of_moves == 0,
    hexagon:axial_neighbors([0, 0], Placements),
    !.
possible_placements(Player_id, Number_of_moves, Placements):-
    % the possible hexagons are the empty neighbors of those of Player_id
    % and that are not neighbors of the other player
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    other_player(Player_id, Other_player_id),
    get_void_neighbors_of_all_hex(Player_id, Void_neighbors),
    findall(Hex_other_player, insect(_, _, Other_player_id, Hex_other_player, true, _), Hexagons),
    hexagon:anti_neighborhood(Void_neighbors, Hexagons, Placements).

% possible moves of any insect
% possible_moves(Player_id, Type, Hexagon, Moves or MSG, Staus_Code)
possible_moves(Player_id, _, _, _, MSG, Status_Code):-
    not(insect(queen_bee, _, Player_id, _, true, 0)),
    MSG = "Add your QUEEN if you want to move!",
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, Id, Hexagon, MSG, Status_Code):-
    insect(Type, Id, Player_id, Hexagon, true, _),
    is_blocked(Type,Id,Player_id,Hexagon),
    MSG = "Insect blocked!",
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, Id, Hexagon, MSG, Status_Code):-
    insect(Type, Id, Player_id, Hexagon, true, _),
    get_hive_hexagons(Hive_hex),
    hexagon:is_a_hinged_hex(Hexagon, Hive_hex),
    MSG = "Cannot move because this would break the hive in 2.",
    Status_Code = 400,
    !.
possible_moves(Player_id, Type, _, Hexagon, Moves, Status_Code):-
    
    switch(Type,
        [
            queen_bee: queen_bee_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            beetle: beetle_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            grasshopper: grasshopper_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            spider: spider_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            soldier_ant: soldier_ant_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            ladybug: ladybug_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            mosquito: mosquito_possible_moves(Player_id, Hexagon, Moves, Status_Code),
            pillbug: pillbug_possible_moves(Player_id, Hexagon, Moves, Status_Code)
        ]).

% switch case {key: value=fun}
switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

% get_possible_move_in_direction(Dir, Hex)
get_possible_move_in_direction(_, Hex, PM, N):-
    N == 0,
    is_an_empty_hex(Hex),
    PM = [],!.
get_possible_move_in_direction(Dir, Hex, PM, N):-
    N == 0,
    not(is_an_empty_hex(Hex)),
    hexagon:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    get_possible_move_in_direction(Dir, NHex, PM, N1).

get_possible_move_in_direction(_, Hex, PM, N):-
    N > 0,
    is_an_empty_hex(Hex),
    PM = Hex,!.
get_possible_move_in_direction(Dir, Hex, PM, N):-
    N > 0,
    not(is_an_empty_hex(Hex)),
    hexagon:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    get_possible_move_in_direction(Dir, NHex, PM, N1).

% queen bee possible moves
queen_bee_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true, 0),
    get_void_neighbors_of_hex_2( Hexagon, VN),
    findall(X, 
        (
            member(X, VN),
            can_move(Hexagon, X)
        ),L),
        L == [],
        MSG = 'Queen bee has no allowed destination.',
        Status_Code = 400,!.
queen_bee_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagon, true, 0),
    get_void_neighbors_of_hex_2(Hexagon, VN),
    findall(X, 
        (
            member(X, VN),
            can_move(Hexagon, X)
        ),Moves),
    Status_Code = 200.

% beetle possible moves
beetle_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            can_move(X, Hexagon)
        ),L1),
    get_placed_neighbors_of_hex(Hexagon, L2),
    append(L1, L2, Moves),
    Moves == [],
    MSG = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetle_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    get_placed_neighbors_of_hex(Hexagon, PN),
    append(VN, PN, Moves),
    Moves == [],
    MSG = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetle_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    findall(X, 
        (
            member(X,VN),
            can_move(Hexagon, X)
        ),L1),
    get_placed_neighbors_of_hex(Hexagon, L2),
    append(L1, L2, Moves),
    Status_Code = 200,!.
beetle_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    get_last_insect(Hexagon, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    get_void_neighbors_of_hex(Player_id, Hexagon, VN),
    get_placed_neighbors_of_hex(Hexagon, PN),
    append(VN, PN, Moves),
    Status_Code = 200,!.

% grasshopper possible moves
grasshopper_possible_moves(Player_id, Hexagon, MSG, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagon, true, 0),
    hexagon:axial_neighbor(Hexagon, 1, NW),
    hexagon:axial_neighbor(Hexagon, 2, W),
    hexagon:axial_neighbor(Hexagon, 3, SW),
    hexagon:axial_neighbor(Hexagon, 4, SE),
    hexagon:axial_neighbor(Hexagon, 5, E),
    hexagon:axial_neighbor(Hexagon, 6, NE),
    
    get_possible_move_in_direction(1, NW, PM1, 0),
    get_possible_move_in_direction(2, W, PM2, 0),
    get_possible_move_in_direction(3, SW, PM3, 0),
    get_possible_move_in_direction(4, SE, PM4, 0),
    get_possible_move_in_direction(5, E, PM5, 0),
    get_possible_move_in_direction(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6] ,[], L),
    utils:delete_all_occurrences([], L ,L1),
    L1 == [],
    MSG = 'Grasshopper bee has no allowed destination.',
    Status_Code = 400,!.
grasshopper_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagon, true, 0),
    hexagon:axial_neighbor(Hexagon, 1, NW),
    hexagon:axial_neighbor(Hexagon, 2, W),
    hexagon:axial_neighbor(Hexagon, 3, SW),
    hexagon:axial_neighbor(Hexagon, 4, SE),
    hexagon:axial_neighbor(Hexagon, 5, E),
    hexagon:axial_neighbor(Hexagon, 6, NE),
    
    get_possible_move_in_direction(1, NW, PM1, 0),
    get_possible_move_in_direction(2, W, PM2, 0),
    get_possible_move_in_direction(3, SW, PM3, 0),
    get_possible_move_in_direction(4, SE, PM4, 0),
    get_possible_move_in_direction(5, E, PM5, 0),
    get_possible_move_in_direction(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6],[], L),
    utils:delete_all_occurrences([],L ,Moves),
    Status_Code = 200.

% spider possible moves
spider_possible_moves(_, Hexagon, Moves, Status_Code):-
    bfs_lvl([[Hexagon, 0]], [], 3, valid_adj1),!,
    findall(U, (node(U, Lvl), Lvl > -1), Moves0),
    retractall(node(_, _)),
    get_void_neighbors_of_hex_2(Hexagon, VN),
    findall(X, (member(X, VN), not(can_move(Hexagon, X))), L),
    utils:delete2(L, Moves0, Moves1),
    filter_spider_moves(Hexagon, Moves1, Moves),
    Status_Code = 200.

% Put in L2 spider possible moves 
filter_spider_moves(Hex, L1, L2):-
    tell('log'),
    findall(Path, (
        member(X, L1), 
        hexagon:dfs_visit([[Hex]], X, Path, _, L1), 
        length(Path, Len), 
        Len == 4, 
        is_valid_path_spider(Path)
        ), Paths),
    get_from_box(),
    findall(H, member([H|_], Paths), L2),
    told.

% Path = p1,p2,p3,p4 is valid if can move from p_{i} to p_{i+1}
is_valid_path_spider(Path):-
    reverse(Path, RPath),
    utils:element_at(X1, RPath, 1),!,
    utils:element_at(X2, RPath, 2),!,
    utils:element_at(X3, RPath, 3),!,
    utils:element_at(X4, RPath, 4),!,
    can_move(X1, X2),
    can_move2(X1, X2, X3),
    can_move2(X1, X3, X4).


% get the Term save on the box
get_from_box():-
    not(box(_)),!.
get_from_box():-
    box(L),
    retract(box(L)),
    T=..L,
    assert(T),!.

% remove predicate P whose list is [name, arg...] and save on the box
move_to_box(L):-
    T=..L,
    retract(T),
    L1 = [box, L],
    T1=..L1,
    assert(T1),!.

% soldier ant possible moves
soldier_ant_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(soldier_ant, _, Player_id, Hexagon, true, _),
    current_prolog_flag(max_tagged_integer, MaxI),
    bfs_lvl([[Hexagon, 0]], [], MaxI, valid_adj1),!,
    findall(U, (node(U, Lvl), Lvl > 0), Moves0),
    retractall(node(_, _)),
    get_void_neighbors_of_hex_2(Hexagon, VN),
    findall(X, (member(X, VN), not(can_move(Hexagon, X))), L),
    utils:delete2(L, Moves0, Moves),
    Status_Code = 200.

% bfs (Queue, Visited, Lvl, AdjPred) | Expand the search as long as it does not exceed the Lvl. Uses AdjPred to find adj hexagons
bfs_lvl([], _, _, _):-!.
bfs_lvl([[_, _lvl]|_], _, Lvl, _):-
    _lvl > Lvl,!.
bfs_lvl([[U, _]|Q], Visited, Lvl, AdjPred):-
    member(U, Visited),
    bfs_lvl(Q, Visited, Lvl, AdjPred).
bfs_lvl([[U, _lvl]|Q], Visited, Lvl, AdjPred):-
    not(member(U, Visited)),
    assert(node(U, _lvl)),
    Pred =..[AdjPred, [U, _lvl], A],
    call(Pred),
    % valid_adj([U, _lvl], A),
    append([U], Visited, Visited1),
    append(Q, A, Q1),
    bfs_lvl(Q1, Visited1, Lvl, AdjPred).

% get all valid adj
valid_adj1([U, Lvl], A):-
    Lvl1 is Lvl + 1,
    get_void_neighbors_of_hex_2(U, N),

    findall([V, Lvl1], 
        (
            member(V,N),
            % can_move(U, V),
            not(road_blocked(U, V)),
            has_at_least_one_neighbor_placed(V)
        ), A).

valid_adj2([U, Lvl], A):-
    Lvl1 is Lvl + 1,
    get_void_neighbors_of_hex_2(U, N),

    findall([V, Lvl1], 
        (
            member(V,N)
            % can_move(U, V),
            % not(road_blocked(U, V)),
            % has_at_least_one_neighbor_placed(V)
        ), A).



ladybug_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(ladybug, _, Player_id, Hexagon, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
mosquito_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(mosquito, _, Player_id, Hexagon, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.
pillbug_possible_moves(Player_id, Hexagon, Moves, Status_Code):-
    insect(pillbug, _, Player_id, Hexagon, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagon, Moves),
    Status_Code = 200.

% get void neighbor of Player_id's Hex(Player_id, Hex, Void_neighbors)
get_void_neighbors_of_hex(Player_id, Hex, Void_neighbors):-
    other_player(Player_id, Other_player_id),
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H1, insect(_, _, Player_id, H1, true,_), Hexagons1),
    findall(H2, insect(_, _, Other_player_id, H2, true,_), Hexagons2),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons1)), not(member(X, Hexagons2))), Void_neighbors).

% get void neighbor of Hex(Hex, Void_neighbors)
get_void_neighbors_of_hex_2(Hex, Void_neighbors):-
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true,_), Hexagons),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons))), Void_neighbors).

% get placed neighbor of Hex(Hex, Void_neighbors)queen bee
get_placed_neighbors_of_hex(Hex, Placed_neighbors):-
    hexagon:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true, 0), Hexagons),
    findall(X, (member(X, Neighbors), member(X, Hexagons)), Placed_neighbors).

% amount of common neihbors | Len = 0 or 1 or 2
amount_common_neighbors(H1, H2, Len):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X,Set), not(is_an_empty_hex(X))),L),
    length(L,Len).
% amount of common neihbors between H1, H2 except Hexagon | Len = 0 or 1
amount_common_neighbors2(Hexagon, H1, H2, Len):-
    hexagon:axial_neighbors(H1,N1),
    hexagon:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X, Set), not(member(Hexagon, Set)), not(is_an_empty_hex(X))),L),
    length(L,Len).

% amount of placed neihbors of Hex
amount_neighbors(Hex, A):-
    get_placed_neighbors_of_hex(Hex, PN),
    length(PN, A),!.

% H2 is blocked to H1 if their two neighbors in common are placed
road_blocked(H1, H2):-
    amount_common_neighbors(H1, H2, Len),
    Len == 2.

% H1 can move to H2 if the have exactly one neighbor in common
can_move(H1, H2):-
    amount_common_neighbors(H1, H2, Len),
    Len == 1.
% same as can_move but using amount_common_neighbors2
can_move2(Hexagon, H1, H2):-
    amount_common_neighbors2(Hexagon, H1, H2, Len),
    Len == 1.

% if U has at least one neighbor placed in the Hive
has_at_least_one_neighbor_placed(U):-
    get_placed_neighbors_of_hex(U, PN),
    length(PN, Len),
    Len > 0.

% returns all insects placed in the hive
get_hive_hexagons(Hive_hex):-
    findall(H, insect(_, _, _, H, true,_), Hive_hex).

% get void neighbor of all Player_id's Hex(Player_id, Void_neighbors)
get_void_neighbors_of_all_hex(Player_id, Void_neighbors):-
    findall(VN, (insect(_, _, Player_id, Hex, true,_), get_void_neighbors_of_hex(Player_id, Hex, VN)), Void_neighbors_aux1),
    utils:flatten_hex(Void_neighbors_aux1, Void_neighbors_aux2),
    setof(X, member(X, Void_neighbors_aux2), Void_neighbors).
% get void neighbor of all Hex(Void_neighbors)
get_void_neighbors_of_all_hex_2(Void_neighbors):-
    findall(VN, (insect(_, _, _, Hex, true,_), get_void_neighbors_of_hex_2(Hex, VN)), Void_neighbors_aux),
    utils:flatten_hex(Void_neighbors_aux, Void_neighbors_aux1),
    setof(X, member(X, Void_neighbors_aux1), Void_neighbors).

% place insect in Hex
% place_insect(Player_id, Type, Hex, Insect)
place_insect(Player_id, Type, Hex, Insect):-
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

% move insect from hexagon_ori to hexagon_end
% move_insect(Player_id, Type, Hex, Insect)
move_insect(Type, Id, Player_id, Lvl, Hexagon_Ori, Hexagon_End, InsectRes):-
    retract(insect(Type, Id, Player_id, Hexagon_Ori, true, Lvl)),

    get_max_lvl_in_hex(Hexagon_End, Lvl_end),

    Lvl1 is Lvl_end + 1,

    assert(insect(Type, Id, Player_id, Hexagon_End, true, Lvl1)),
    InsectRes = [Type, Id, Player_id, Hexagon_End, true, Lvl1],!.


% all insect with filter
all_insects(Type, Id, Player_id, Hex, Placed, Lvl, Insects):-
    findall([Type, Id, Player_id, Hex, Placed, Lvl], insect(Type, Id, Player_id, Hex, Placed, Lvl), Insects).
% initialize insects with default values
init_insects():-
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

% if Hex is empty
is_an_empty_hex(Hex):-
    not(insect(_, _, _, Hex, true,_)),!.

% an insect is blocked if there is another insect with a hingher level in the same hexagon
is_blocked(Type,Id,Pid,Hex):-
    insect(Type, Id, Pid, Hex, true, Lvl),
    Lvl1 is Lvl+1,
    insect(_, _, _, Hex, true, Lvl1),!.

% return max lvl in Hex
get_max_lvl_in_hex(Hex, Lvl):-
    is_an_empty_hex(Hex),
    Lvl is -1,!.
get_max_lvl_in_hex(Hex, Lvl):-
    findall(Lvl1, insect(_,_,_,Hex,true,Lvl1), L),
    utils:last_element(Lvl,L).

% get the insect with the max lvl in Hex
get_last_insect(Hex, I):-
    is_an_empty_hex(Hex),
    I=[],!.
get_last_insect(Hex, I):-
    get_max_lvl_in_hex(Hex, Lvl),
    insect(Type, Id, Pid, Hex, true, Lvl),
    I = [Type, Id, Pid, Hex, true, Lvl].