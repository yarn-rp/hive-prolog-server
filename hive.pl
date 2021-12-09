% --------------------------------------MODULES--------------------------------------
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

% --------------------------------------URL HANDLERS--------------------------------------
:- http_handler('/hive_api/ping', handle_request_ping_pong, []).
:- http_handler('/hive_api/insect/get_possible_placements', handle_request_get_possible_placements, []).
:- http_handler('/hive_api/insect/get_possible_moves', handle_request_get_possible_moves, []).
:- http_handler('/hive_api/insect/cambiar_insecto', handle_request_place_insect, []).
:- http_handler('/hive_api/insect/move_insect', handle_request_move_insect, []).
:- http_handler('/hive_api/insect/get_last', handle_request_get_last, []).
:- http_handler('/hive_api/game/game_stats', handle_request_game_stats, []).
:- http_handler('/hive_api/game/reset_game', handle_request_reset_game, []).
:- http_handler('/hive_api/game/new_game', handle_request_new_game, []).
:- http_handler('/hive_api/insect/queen_surrounded', handle_request_queen_surrounded, []).

% get_last_insect
% --------------------------------------METHODS--------------------------------------
% ping_pong | Tester server
ping_pong(_{status_code:Status_Code, ping_mensaje:PinMSG}) :-
    Status_Code = 200,
    PinMSG = "pong".

% Place insect
poner_insecto(_{tipo:Tipo, hexagono:Hexagono}, _{status_code:Status_Code, mensaje:Mensaje}) :-
    string_to_atom(Tipo, Tipo_atomico),
    (not(compound(Hexagono)); not(atom(Tipo_atomico))),
    Status_Code = 400,
    Mensaje = "Wrong params!",
    !.
poner_insecto(_{tipo:_, hexagono:Hexagono}, _{status_code:Status_Code, mensaje:Mensaje}) :-
    jugador_actual(Player_id),
    jugador(Player_id, _, Number_of_moves, _, _, _),
    posibles_jugadas(Player_id, Number_of_moves, Placements),
    not(member(Hexagono, Placements)),
    Status_Code = 400,
    Mensaje = "Wrong placement",
    !.
poner_insecto(_{tipo:Tipo, hexagono:_}, _{status_code:Status_Code, mensaje:Mensaje}) :-
    string_to_atom(Tipo, Tipo_atomico),
    not(insect(Tipo_atomico, _, _, _, _,_)),
    Status_Code = 400,
    Mensaje = "Nonexistent insect",
    !.
poner_insecto(_{tipo:Tipo, hexagono:Hexagono}, _{status_code:Status_Code, insect:Insect}) :-
    string_to_atom(Tipo, Tipo_atomico),
    jugador_actual(Player_id),
    Tipo_atomico == queen_bee,
    cambiar_insecto(Player_id, Tipo_atomico, Hexagono, Insect),
    place_queen_bee(Player_id),
    increment_number_of_moves(Player_id),
    next_player(),
    Status_Code = 200,
    !.
poner_insecto(_{tipo:Tipo, hexagono:Hexagono}, _{status_code:Status_Code, insect:Insect}) :-
    string_to_atom(Tipo, Tipo_atomico),
    jugador_actual(Player_id),
    cambiar_insecto(Player_id, Tipo_atomico, Hexagono, Insect),
    increment_number_of_moves(Player_id),
    next_player(),
    Status_Code = 200.

% move insect
moveInsect(_{tipo:Tipo, hexagon_ori:Hexagon_Ori, hexagon_end:Hexagon_End}, _{status_code:Status_Code, mensaje:Mensaje}) :-
    (not(compound(Hexagon_Ori)); not(compound(Hexagon_End)); not(atom(Tipo))),
    Status_Code = 400,
    Mensaje = "Wrong params!",
    !.
moveInsect(_{tipo:Tipo, hexagon_ori:Hexagon_Ori, hexagon_end:Hexagon_End}, _{status_code:Status_Code, mensaje:Mensaje}) :-
    string_to_atom(Tipo, TypeA),
    jugador_actual(Player_id),
    jugador(Player_id, _, _, _, _, _),
    possible_moves(Player_id, TypeA,_,  Hexagon_Ori, Moves, Status_Code),
    % Player_id, Tipo_atomico, Id, Hexagono, Mensaje, Status_Code
    not(member(Hexagon_End, Moves)),
    Status_Code = 400,
    Mensaje = "Wrong placement",
    !.
moveInsect(_{tipo:Tipo, id:Id, lvl:Lvl, hexagon_ori:Hexagon_Ori, hexagon_end:Hexagon_End}, _{status_code:Status_Code, insectRes:InsectRes}):-
    string_to_atom(Tipo, TypeA),
    jugador_actual(Player_id),
    move_insect(TypeA, Id, Player_id, Lvl, Hexagon_Ori, Hexagon_End, InsectRes),
    increment_number_of_moves(Player_id),
    next_player(),
    Status_Code = 200.

% get the last insect in hexagono
getLast(_{hexagono:Hexagono}, _{status_code:Status_Code ,mensaje:Mensaje}):-
    get_last_insect(Hexagono, Insect),
    Insect == [],
    Mensaje = "Empty hexagono!",
    Status_Code = 400,!.
getLast(_{hexagono:Hexagono}, _{status_code:Status_Code ,insect:Insect}):-
    get_last_insect(Hexagono, Insect),
    Status_Code = 200.

% Get possible placements
% getPossiblePlacements(_{tipo:_}, _{status_code:Status_Code, mensaje:Mensaje}):-
%     jugador_actual(Player_id),
%     jugador(Player_id, Name, Number_of_moves, _, _, _),
%     not(can_play(Player_id, Name, Number_of_moves)),
%     string_concat(Name, " cannot play and needs to pass its turn.", Mensaje),
%     Status_Code = 401,
%     next_player(),!.
getPossiblePlacements(_{tipo:_}, _{status_code:Status_Code, mensaje:Mensaje}):-
    jugador_actual(Player_id),
    jugador(Player_id, _, Number_of_moves, _, _, _),
    posibles_jugadas(Player_id, Number_of_moves, Placements),
    Placements == [],
    Mensaje = "There is unfortunately nowhere where you are allowed to add a new pawn.",
    Status_Code = 400,
    !.
getPossiblePlacements(_{tipo:Tipo}, _{status_code:Status_Code, placements:Placements}):-
    string_to_atom(Tipo, Tipo_atomico),
    jugador_actual(Player_id),
    jugador(Player_id, _, Number_of_moves, Queen_bee_placed, _, _),
    Queen_bee_placed == false,
    Number_of_moves == 3,
    Tipo_atomico == queen_bee,
    posibles_jugadas(Player_id, Number_of_moves, Placements),
    Status_Code = 200,
    !.
getPossiblePlacements(_, _{status_code:Status_Code, mensaje:Mensaje}):-
    jugador_actual(Player_id),
    jugador(Player_id, _, Number_of_moves, Queen_bee_placed, _, _),
    Queen_bee_placed == false,
    Number_of_moves == 3,
    Status_Code = 400,
    Mensaje = "4th move: you have to add your queen!",
    !.
getPossiblePlacements(_, _{status_code:Status_Code, placements:Placements}) :-
    jugador_actual(Player_id),
    jugador(Player_id, _, Number_of_moves, _, _, _),
    posibles_jugadas(Player_id, Number_of_moves, Placements),
    Status_Code = 200.

% Get possible moves
getPossibleMoves(_{tipo:Tipo, id:Id,  hexagono:Hexagono}, _{status_code:Status_Code, mensaje:Mensaje}):-
    string_to_atom(Tipo, Tipo_atomico),
    jugador_actual(Player_id),
    jugador(Player_id, _, _, _, _, _),
    possible_moves(Player_id, Tipo_atomico, Id, Hexagono, Mensaje, Status_Code),
    Status_Code == 400,
    !.
getPossibleMoves(_{tipo:Tipo, id:Id,  hexagono:Hexagono}, _{status_code:Status_Code, mensaje:Mensaje}):-
    string_to_atom(Tipo, Tipo_atomico),
    jugador_actual(Player_id),
    jugador(Player_id, _, _, _, _, _),
    possible_moves(Player_id, Tipo_atomico, Id, Hexagono, Mensaje, Status_Code),
    Status_Code == 400,
    !.
getPossibleMoves(_{tipo:Tipo, id:Id, hexagono:Hexagono}, _{status_code:Status_Code, moves:Moves}):-
    string_to_atom(Tipo, Tipo_atomico),
    jugador_actual(Player_id),
    jugador(Player_id, _, _, _, _, _),
    possible_moves(Player_id, Tipo_atomico, Id, Hexagono, Moves, Status_Code).

% quee surrounded
queenSurrounded(_{status_code:Status_Code, mensaje:Mensaje}):-
    not(queen_surrounded(p1)),
    not(queen_surrounded(p2)),
    Mensaje = "No queen locked.",
    Status_Code = 200,!.
queenSurrounded(_{status_code:Status_Code, mensaje:Mensaje}):-
    queen_surrounded(p1),
    queen_surrounded(p2),
    Mensaje = "Both queens locked up.",
    Status_Code = 201,!.
queenSurrounded(_{status_code:Status_Code, mensaje:Mensaje}):-
    queen_surrounded(p1),
    Mensaje = "p1's queen blocked.",
    Status_Code = 202,!.
queenSurrounded(_{status_code:Status_Code, mensaje:Mensaje}):-
    queen_surrounded(p2),
    Mensaje = "p2's queen blocked.",
    Status_Code = 203,!.

% New Game
newGame(_{mode:Mode, level:Level}, _{mensaje:Mensaje}):-
    string_to_atom(Mode, ModeAtom),
    new_game(ModeAtom, Level, Mensaje).

% --------------------------------------Request Handlers--------------------------------------
% Handle ping pong
handle_request_ping_pong(_) :-
    ping_pong(Res),
    reply_json_dict(Res).

% Handle place insect
handle_request_place_insect(Req) :-
    http_read_json_dict(Req, Query),
    poner_insecto(Query, Res),
    reply_json_dict(Res).

handle_request_move_insect(Req):-
    http_read_json_dict(Req, Query),
    moveInsect(Query, Res),
    reply_json_dict(Res).

% Handle get possible placements
handle_request_get_possible_placements(Req) :-
    http_read_json_dict(Req, Query),
    getPossiblePlacements(Query, Placements),
    Res = Placements,
    reply_json_dict(Res).

% handle get possible moves
handle_request_get_possible_moves(Req):-
    http_read_json_dict(Req, Query),
    getPossibleMoves(Query, Res),
    reply_json_dict(Res).

% handle get last insect in hexagono
handle_request_get_last(Req):-
    http_read_json_dict(Req, Query),
    getLast(Query, Res),
    reply_json_dict(Res).

% Handle game stats
handle_request_game_stats(_):-
    jugador_actual(Current_player_id),
    jugador(p1, Name_p1, Number_of_moves_p1, Queen_bee_placed_p1, Type_player1, Game_over1),
    jugador(p2, Name_p2, Number_of_moves_p2, Queen_bee_placed_p2, Type_player2, Game_over2),

    all_insects(_, _, p1, _, false,_, Non_placed_insects_p1),
    all_insects(_, _, p2, _, false,_, Non_placed_insects_p2),

    all_insects(_, _, _, _, true,_,Placed_insects),
    

    Players_info =
    _{
        p1:_{
            id:p1, name:Name_p1,    
            number_of_moves:Number_of_moves_p1,
            queen_bee_placed:Queen_bee_placed_p1,
            non_placed_insects: Non_placed_insects_p1,
            type_player: Type_player1,
            game_over: Game_over1
            },
        p2:_{
            id:p2, name:Name_p2,
            number_of_moves:Number_of_moves_p2,
            queen_bee_placed:Queen_bee_placed_p2,
            non_placed_insects: Non_placed_insects_p2,
            type_player: Type_player2,
            game_over: Game_over2
            }
    },

    Hive = Placed_insects,

    Status_Code = 200,

    Res = _{status_code:Status_Code, current_player_id:Current_player_id, players_info:Players_info, hive:Hive},
    reply_json_dict(Res).

% handle queen sorrounded
handle_request_queen_surrounded(Req):-
    queenSurrounded(Res),
    reply_json_dict(Res).

% handle reset game
handle_request_reset_game(_):-
    reset_game(Mensaje),
    Res = _{mensaje:Mensaje},
    reply_json_dict(Res).

% handle new game
handle_request_new_game(Req):-
    http_read_json_dict(Req, Query),
    newGame(Query, Res),
    reply_json_dict(Res).
% --------------------------------------Start server--------------------------------------
start_server(Port) :-
    init_game(pvp, 0),%by default mode=pvp, lvl=0(beginner)
    http_server(http_dispatch, [port(Port)]).

% --------------------------------------Initialization--------------------------------------
:- initialization(start_server(3030), program).



%%%%%%%%% Juego




% --------------------------------------DYNAMICS--------------------------------------
:-dynamic jugador/6, jugador_actual/1, mode/1, level/1.
% jugador(Id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over) | Player Info | p1 is the first jugador and p2 is the second
% jugador_actual(Id) | The current jugador' is

% --------------------------------------METHODS--------------------------------------
% init game
init_game(Mode, Level):-
    Mode == pvp,
    assert(mode(Mode)),
    assert(level(Level)),
    assert(jugador(p1, 'Player 1', 0, false, human, false)),
    assert(jugador(p2, 'Player 2', 0, false, human, false)),
    assert(jugador_actual(p1)),
    init_insects(),!.
init_game(Mode, Level):-
    Mode == pvai,
    assert(mode(Mode)),
    assert(level(Level)),
    assert(jugador(p1, 'Player 1', 0, false, human, false)),
    assert(jugador(p2, 'AI', 0, false, ai, false)),
    assert(jugador_actual(p1)),
    init_insects().

% increment number of moves(Player_id):-
increment_number_of_moves(Player_id):-
    jugador(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over),
    Number_of_moves1 is Number_of_moves + 1,
    retract(jugador(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over)),
    assert(jugador(Player_id, Name, Number_of_moves1, Queen_bee_placed, Type_player, Game_over)).

% reset game
reset_game(Mensaje):-
    mode(Mode),
    level(Level),
    retractall(mode(_)),
    retractall(level(_)),
    retractall(jugador(_,_,_,_,_,_)),
    retractall(jugador_actual(_)),
    retractall(insect(_,_,_,_,_,_)),
    init_game(Mode, Level),
    Mensaje = "ok".

% new game
new_game(Mode, Level, Mensaje):-
    retractall(mode(_)),
    retractall(level(_)),
    retractall(jugador(_,_,_,_,_,_)),
    retractall(jugador_actual(_)),
    retractall(insect(_,_,_,_,_,_)),
    init_game(Mode, Level),
    Mensaje = "ok".

% next jugador
next_player():-
    jugador_actual(P),
    P == p1,
    retract(jugador_actual(p1)),
    assert(jugador_actual(p2)),!.
next_player():-
    jugador_actual(P),
    P == p2,
    retract(jugador_actual(p2)),
    assert(jugador_actual(p1)).

% place queen bee 
place_queen_bee(Player_id):-
    jugador(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over),
    retract(jugador(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over)),
    assert(jugador(Player_id, Name, Number_of_moves, true, Type_player, Game_over)).



%%%%% Hexagono


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
    element_at(Vec, Axial_direction_vectors, Dir).

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

% if Hex is a neighbor of any hexagono in Hexs
be_a_neighbor(Hex, Hexs):-
    member(H, Hexs),
    are_neighbors(Hex, H),!.

% get_QR(Hexagono)
% returns the q and r of a hexagono
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

% a hexagono is hinged if it is a hinge node in the graph formed by all hexagons...
% I remove the candidate hexagono to point of articulation 
% and analyze if all its neighbors are still connected
is_a_hinged_hex(Hex,Hive):-
    axial_neighbors_in_hive(Hex,Hive,L),
    member(H1,L),
    member(H2,L),
    H1 \= H2,
    H1 \= Hex,
    H2 \= Hex,
    delete(Hex,Hive,Hive1),
    not(is_connected(H1,H2,Hive1)),
    !.



%%%%%%%% Insecto


% --------------------------------------DYNAMICS--------------------------------------
:-dynamic insect/6, box/1, node/2.
% box(L). %is to save one predicate temporarily
% insect(Tipo, Id, PlayerId, Hex=[Q,R], Placed, Lvl)
% node(Hex, Lvl in Bfs) is used by bfs_lvl

% other jugador id
other_player(p1, p2).
other_player(p2 ,p1).

% --------------------------------------METHODS--------------------------------------
% can play | Analizes if any piece can be moved or placed and in case it cannot be passed the turn
can_play(Player_id, Name, Number_of_moves):-
    posibles_jugadas(Player_id, Number_of_moves, Placements),
    Placements == [], fail,!.
% can_play(Player_id, Name, Number_of_moves):-
%     posibles_jugadas(Player_id, Number_of_moves, Placements),
%     Placements == [], fail,!.
can_play(_, _, _):-!.

% it is fulfilled if the queen bee is sorrounded
queen_surrounded(Player_id):-
    insect(queen_bee, _, Player_id, Hex, true, 0),
    amount_neighbors(Hex, Len),
    Len == 6.


% possibles placements
% posibles_jugadas(Player_id, Number_of_moves, Placements)
posibles_jugadas(Player_id, Number_of_moves, Placements):-
    % the first jugador's first move will always play on hexagono(0,0)
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p1,
    Number_of_moves == 0,
    Placements = [[0, 0]],
    !.
posibles_jugadas(Player_id, Number_of_moves, Placements):-
    % the second jugador's first move will always play on an neighbor hexagono of hexagono(0,0)
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    Player_id == p2,
    Number_of_moves == 0,
    hexagono:axial_neighbors([0, 0], Placements),
    !.
posibles_jugadas(Player_id, Number_of_moves, Placements):-
    % the possible hexagons are the empty neighbors of those of Player_id
    % and that are not neighbors of the other jugador
    atom(Player_id),
    number(Number_of_moves),
    var(Placements),
    other_player(Player_id, Other_player_id),
    get_void_neighbors_of_all_hex(Player_id, Void_neighbors),
    findall(Hex_other_player, insect(_, _, Other_player_id, Hex_other_player, true, _), Hexagons),
    hexagono:anti_neighborhood(Void_neighbors, Hexagons, Placements).

% possible moves of any insect
% possible_moves(Player_id, Tipo, Hexagono, Moves or Mensaje, Staus_Code)
possible_moves(Player_id, _, _, _, Mensaje, Status_Code):-
    not(insect(queen_bee, _, Player_id, _, true, 0)),
    Mensaje = "Add your QUEEN if you want to move!",
    Status_Code = 400,
    !.
possible_moves(Player_id, Tipo, Id, Hexagono, Mensaje, Status_Code):-
    insect(Tipo, Id, Player_id, Hexagono, true, _),
    is_blocked(Tipo,Id,Player_id,Hexagono),
    Mensaje = "Insect blocked!",
    Status_Code = 400,
    !.
possible_moves(Player_id, Tipo, Id, Hexagono, Mensaje, Status_Code):-
    insect(Tipo, Id, Player_id, Hexagono, true, _),
    get_hive_hexagons(Hive_hex),
    hexagono:is_a_hinged_hex(Hexagono, Hive_hex),
    Mensaje = "Cannot move because this would break the hive in 2.",
    Status_Code = 400,
    !.
possible_moves(Player_id, Tipo, _, Hexagono, Moves, Status_Code):-
    
    switch(Tipo,
        [
            queen_bee: queen_bee_possible_moves(Player_id, Hexagono, Moves, Status_Code),
            beetle: beetle_possible_moves(Player_id, Hexagono, Moves, Status_Code),
            grasshopper: grasshopper_possible_moves(Player_id, Hexagono, Moves, Status_Code),
            spider: spider_possible_moves(Player_id, Hexagono, Moves, Status_Code),
            soldier_ant: soldier_ant_possible_moves(Player_id, Hexagono, Moves, Status_Code),
            ladybug: ladybug_possible_moves(Player_id, Hexagono, Moves, Status_Code),
            mosquito: mosquito_possible_moves(Player_id, Hexagono, Moves, Status_Code),
            pillbug: pillbug_possible_moves(Player_id, Hexagono, Moves, Status_Code)
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
    hexagono:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    get_possible_move_in_direction(Dir, NHex, PM, N1).

get_possible_move_in_direction(_, Hex, PM, N):-
    N > 0,
    is_an_empty_hex(Hex),
    PM = Hex,!.
get_possible_move_in_direction(Dir, Hex, PM, N):-
    N > 0,
    not(is_an_empty_hex(Hex)),
    hexagono:axial_neighbor(Hex, Dir, NHex),
    N1 is N+1,
    get_possible_move_in_direction(Dir, NHex, PM, N1).

% queen bee possible moves
queen_bee_possible_moves(Player_id, Hexagono, Mensaje, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagono, true, 0),
    get_void_neighbors_of_hex_2( Hexagono, VN),
    findall(X, 
        (
            member(X, VN),
            can_move(Hexagono, X)
        ),L),
        L == [],
        Mensaje = 'Queen bee has no allowed destination.',
        Status_Code = 400,!.
queen_bee_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    insect(queen_bee, _, Player_id, Hexagono, true, 0),
    get_void_neighbors_of_hex_2(Hexagono, VN),
    findall(X, 
        (
            member(X, VN),
            can_move(Hexagono, X)
        ),Moves),
    Status_Code = 200.

% beetle possible moves
beetle_possible_moves(Player_id, Hexagono, Mensaje, Status_Code):-
    get_last_insect(Hexagono, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    get_void_neighbors_of_hex(Player_id, Hexagono, VN),
    findall(X, 
        (
            member(X,VN),
            can_move(X, Hexagono)
        ),L1),
    get_placed_neighbors_of_hex(Hexagono, L2),
    append(L1, L2, Moves),
    Moves == [],
    Mensaje = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetle_possible_moves(Player_id, Hexagono, Mensaje, Status_Code):-
    get_last_insect(Hexagono, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    get_void_neighbors_of_hex(Player_id, Hexagono, VN),
    get_placed_neighbors_of_hex(Hexagono, PN),
    append(VN, PN, Moves),
    Moves == [],
    Mensaje = 'Beetle has no allowed destination.',
    Status_Code = 400,!.
beetle_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    get_last_insect(Hexagono, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl == 0,
    get_void_neighbors_of_hex(Player_id, Hexagono, VN),
    findall(X, 
        (
            member(X,VN),
            can_move(Hexagono, X)
        ),L1),
    get_placed_neighbors_of_hex(Hexagono, L2),
    append(L1, L2, Moves),
    Status_Code = 200,!.
beetle_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    get_last_insect(Hexagono, I),
    [_, _, Pid, _, true, Lvl] = I,
    Player_id == Pid,
    Lvl > 0,
    get_void_neighbors_of_hex(Player_id, Hexagono, VN),
    get_placed_neighbors_of_hex(Hexagono, PN),
    append(VN, PN, Moves),
    Status_Code = 200,!.

% grasshopper possible moves
grasshopper_possible_moves(Player_id, Hexagono, Mensaje, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagono, true, 0),
    hexagono:axial_neighbor(Hexagono, 1, NW),
    hexagono:axial_neighbor(Hexagono, 2, W),
    hexagono:axial_neighbor(Hexagono, 3, SW),
    hexagono:axial_neighbor(Hexagono, 4, SE),
    hexagono:axial_neighbor(Hexagono, 5, E),
    hexagono:axial_neighbor(Hexagono, 6, NE),
    
    get_possible_move_in_direction(1, NW, PM1, 0),
    get_possible_move_in_direction(2, W, PM2, 0),
    get_possible_move_in_direction(3, SW, PM3, 0),
    get_possible_move_in_direction(4, SE, PM4, 0),
    get_possible_move_in_direction(5, E, PM5, 0),
    get_possible_move_in_direction(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6] ,[], L),
    delete_all_occurrences([], L ,L1),
    L1 == [],
    Mensaje = 'Grasshopper bee has no allowed destination.',
    Status_Code = 400,!.
grasshopper_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    insect(grasshopper, _, Player_id, Hexagono, true, 0),
    hexagono:axial_neighbor(Hexagono, 1, NW),
    hexagono:axial_neighbor(Hexagono, 2, W),
    hexagono:axial_neighbor(Hexagono, 3, SW),
    hexagono:axial_neighbor(Hexagono, 4, SE),
    hexagono:axial_neighbor(Hexagono, 5, E),
    hexagono:axial_neighbor(Hexagono, 6, NE),
    
    get_possible_move_in_direction(1, NW, PM1, 0),
    get_possible_move_in_direction(2, W, PM2, 0),
    get_possible_move_in_direction(3, SW, PM3, 0),
    get_possible_move_in_direction(4, SE, PM4, 0),
    get_possible_move_in_direction(5, E, PM5, 0),
    get_possible_move_in_direction(6, NE, PM6, 0),
    
    union([PM1,PM2,PM3,PM4,PM5,PM6],[], L),
    delete_all_occurrences([],L ,Moves),
    Status_Code = 200.

% spider possible moves
spider_possible_moves(_, Hexagono, Moves, Status_Code):-
    bfs_lvl([[Hexagono, 0]], [], 3, valid_adj1),!,
    findall(U, (node(U, Lvl), Lvl > -1), Moves0),
    retractall(node(_, _)),
    get_void_neighbors_of_hex_2(Hexagono, VN),
    findall(X, (member(X, VN), not(can_move(Hexagono, X))), L),
    delete2(L, Moves0, Moves1),
    filter_spider_moves(Hexagono, Moves1, Moves),
    Status_Code = 200.

% Put in L2 spider possible moves 
filter_spider_moves(Hex, L1, L2):-
    tell('log'),
    findall(Path, (
        member(X, L1), 
        hexagono:dfs_visit([[Hex]], X, Path, _, L1), 
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
    element_at(X1, RPath, 1),!,
    element_at(X2, RPath, 2),!,
    element_at(X3, RPath, 3),!,
    element_at(X4, RPath, 4),!,
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
soldier_ant_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    insect(soldier_ant, _, Player_id, Hexagono, true, _),
    current_prolog_flag(max_tagged_integer, MaxI),
    bfs_lvl([[Hexagono, 0]], [], MaxI, valid_adj1),!,
    findall(U, (node(U, Lvl), Lvl > 0), Moves0),
    retractall(node(_, _)),
    get_void_neighbors_of_hex_2(Hexagono, VN),
    findall(X, (member(X, VN), not(can_move(Hexagono, X))), L),
    delete2(L, Moves0, Moves),
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



ladybug_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    insect(ladybug, _, Player_id, Hexagono, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagono, Moves),
    Status_Code = 200.
mosquito_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    insect(mosquito, _, Player_id, Hexagono, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagono, Moves),
    Status_Code = 200.
pillbug_possible_moves(Player_id, Hexagono, Moves, Status_Code):-
    insect(pillbug, _, Player_id, Hexagono, true,_),
    get_void_neighbors_of_hex(Player_id, Hexagono, Moves),
    Status_Code = 200.

% get void neighbor of Player_id's Hex(Player_id, Hex, Void_neighbors)
get_void_neighbors_of_hex(Player_id, Hex, Void_neighbors):-
    other_player(Player_id, Other_player_id),
    hexagono:axial_neighbors(Hex, Neighbors),
    findall(H1, insect(_, _, Player_id, H1, true,_), Hexagons1),
    findall(H2, insect(_, _, Other_player_id, H2, true,_), Hexagons2),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons1)), not(member(X, Hexagons2))), Void_neighbors).

% get void neighbor of Hex(Hex, Void_neighbors)
get_void_neighbors_of_hex_2(Hex, Void_neighbors):-
    hexagono:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true,_), Hexagons),
    findall(X, (member(X, Neighbors), not(member(X, Hexagons))), Void_neighbors).

% get placed neighbor of Hex(Hex, Void_neighbors)queen bee
get_placed_neighbors_of_hex(Hex, Placed_neighbors):-
    hexagono:axial_neighbors(Hex, Neighbors),
    findall(H, insect(_, _, _, H, true, 0), Hexagons),
    findall(X, (member(X, Neighbors), member(X, Hexagons)), Placed_neighbors).

% amount of common neihbors | Len = 0 or 1 or 2
amount_common_neighbors(H1, H2, Len):-
    hexagono:axial_neighbors(H1,N1),
    hexagono:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X,Set), not(is_an_empty_hex(X))),L),
    length(L,Len).
% amount of common neihbors between H1, H2 except Hexagono | Len = 0 or 1
amount_common_neighbors2(Hexagono, H1, H2, Len):-
    hexagono:axial_neighbors(H1,N1),
    hexagono:axial_neighbors(H2,N2),
    intersection(N1, N2, Set),
    findall(X, (member(X, Set), not(member(Hexagono, Set)), not(is_an_empty_hex(X))),L),
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
can_move2(Hexagono, H1, H2):-
    amount_common_neighbors2(Hexagono, H1, H2, Len),
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
    flatten_hex(Void_neighbors_aux1, Void_neighbors_aux2),
    setof(X, member(X, Void_neighbors_aux2), Void_neighbors).
% get void neighbor of all Hex(Void_neighbors)
get_void_neighbors_of_all_hex_2(Void_neighbors):-
    findall(VN, (insect(_, _, _, Hex, true,_), get_void_neighbors_of_hex_2(Hex, VN)), Void_neighbors_aux),
    flatten_hex(Void_neighbors_aux, Void_neighbors_aux1),
    setof(X, member(X, Void_neighbors_aux1), Void_neighbors).

% place insect in Hex
% cambiar_insecto(Player_id, Tipo, Hex, Insect)
cambiar_insecto(Player_id, Tipo, Hex, Insect):-
    var(Insect),
    atom(Player_id),
    atom(Tipo),
    compound(Hex),
    var(Insect),
    insect(Tipo, Id, Player_id, none, false,-1),
    !,
    retract(insect(Tipo, Id, Player_id, none, false,-1)),
    assert(insect(Tipo, Id, Player_id, Hex, true,0)),
    insect(Tipo, Id, Player_id, Hex, true,0)=..Insect.

% move insect from hexagon_ori to hexagon_end
% move_insect(Player_id, Tipo, Hex, Insect)
move_insect(Tipo, Id, Player_id, Lvl, Hexagon_Ori, Hexagon_End, InsectRes):-
    retract(insect(Tipo, Id, Player_id, Hexagon_Ori, true, Lvl)),

    get_max_lvl_in_hex(Hexagon_End, Lvl_end),

    Lvl1 is Lvl_end + 1,

    assert(insect(Tipo, Id, Player_id, Hexagon_End, true, Lvl1)),
    InsectRes = [Tipo, Id, Player_id, Hexagon_End, true, Lvl1],!.


% all insect with filter
all_insects(Tipo, Id, Player_id, Hex, Placed, Lvl, Insects):-
    findall([Tipo, Id, Player_id, Hex, Placed, Lvl], insect(Tipo, Id, Player_id, Hex, Placed, Lvl), Insects).
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

% an insect is blocked if there is another insect with a hingher level in the same hexagono
is_blocked(Tipo,Id,Pid,Hex):-
    insect(Tipo, Id, Pid, Hex, true, Lvl),
    Lvl1 is Lvl+1,
    insect(_, _, _, Hex, true, Lvl1),!.

% return max lvl in Hex
get_max_lvl_in_hex(Hex, Lvl):-
    is_an_empty_hex(Hex),
    Lvl is -1,!.
get_max_lvl_in_hex(Hex, Lvl):-
    findall(Lvl1, insect(_,_,_,Hex,true,Lvl1), L),
    last_element(Lvl,L).

% get the insect with the max lvl in Hex
get_last_insect(Hex, I):-
    is_an_empty_hex(Hex),
    I=[],!.
get_last_insect(Hex, I):-
    get_max_lvl_in_hex(Hex, Lvl),
    insect(Tipo, Id, Pid, Hex, true, Lvl),
    I = [Tipo, Id, Pid, Hex, true, Lvl].


% --------------------------------------METHODS--------------------------------------
% element_at(X, L, I).
element_at(X,[X|_],1).
element_at(X,[_|L],K) :- element_at(X,L,K1), K is K1 + 1.

% indexOf(X,L,I)
indexOf(X, [X|_], 1):-!.
indexOf(X, [_|R], I):-
    indexOf(X, R, I1),
    !,
    I is I1+1.

% concat(L1, L2, L3) | L3 = L1 + L2
concat([], X, X):-!.
concat([X|R], Y, [X|Z]):-
    concat(R, Y, Z).

% isList(L)
isList([]).
isList([_|_]).

% isHex(Hex)
isHex([Q, R]):-
    number(Q),
    number(R).

% element_hex(Hex, L)
element_hex(X, [X]):-
    isHex(X),!.
element_hex(X, [X]):-
    not(isList(X)).
element_hex(X, V):-
    isList(X),
    flatten_hex(X,V).

% flatten_hex(L1, L2).
%Ex: flatten_hex([ [[1,2],[3,4]], [[5,6],[7,8]] ], [[1,2],[3,4],[5,6],[7,8]]).
flatten_hex([],[]).
flatten_hex([X|Y],R):- 
    element_hex(X,V),
    flatten_hex(Y,L),
    concat(V,L,R).

% Delete(X, L, R)
delete(X,[X|R],R).
delete(X,[Y|R],[Y|R1]):-delete(X,R,R1).

% L3 is the difference of L2 and l1.
delete2(L1, L2, L3):-
    findall(X, (member(X,L2), not(member(X,L1))), L3).

% delete_all_occurrences(X,L,R)
delete_all_occurrences(_,[],[]).
delete_all_occurrences(X,[X|T],R):-
    delete_all_occurrences(X,T,R),!.
delete_all_occurrences(X,[Y|R],[Y|R1]):-
    delete_all_occurrences(X,R,R1).

% last element
last_element(X,[X|[]]):-!.
last_element(X,[_|T]):-
    last_element(X,T).
