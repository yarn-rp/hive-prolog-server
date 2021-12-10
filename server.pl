% ##################################################################################
% --------------------------------------SERVER--------------------------------------
% ##################################################################################

% --------------------------------------EXPORTS--------------------------------------
% ...

% --------------------------------------MODULES--------------------------------------
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- consult(game), import(game).

% --------------------------------------URL HANDLERS--------------------------------------
:- http_handler('/api/ping', handle_request_ping_pong, []).
:- http_handler('/api/insect/get_possible_placements', handle_request_get_possible_placements, []).
:- http_handler('/api/insect/get_possible_moves', handle_request_get_possible_moves, []).
:- http_handler('/api/movePieceFromHand', handle_request_place_insect, []).
:- http_handler('/api/movePieceFromBoard', handle_request_move_insect, []).
:- http_handler('/api/newGame', handle_request_new_game, []).
:- http_handler('/api/insect/queen_surrounded', handle_request_queen_surrounded, []).

% get_last_insect
% --------------------------------------METHODS--------------------------------------
% ping_pong | Tester server
ping_pong(_{status_code:Status_Code, msg:Msg}) :-
    Status_Code = 200,
    Msg = "pong".

% Place insect
placeInsect(_{type:Type, hexagon:Hexagon}, _{status_code:Status_Code, msg:MSG}) :-
    string_to_atom(Type, Type_atom),
    (not(compound(Hexagon)); not(atom(Type_atom))),
    Status_Code = 400,
    MSG = "Wrong params!",
    !.
placeInsect(_{type:_, hexagon:Hexagon}, _{status_code:Status_Code, msg:MSG}) :-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, _, _, _),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    not(member(Hexagon, Placements)),
    Status_Code = 400,
    MSG = "Wrong placement",
    !.
placeInsect(_{type:Type, hexagon:_}, _{status_code:Status_Code, msg:MSG}) :-
    string_to_atom(Type, Type_atom),
    not(game:insects:insect(Type_atom, _, _, _, _,_)),
    Status_Code = 400,
    MSG = "Nonexistent insect",
    !.
placeInsect(_{type:Type, hexagon:Hexagon}, _{status_code:Status_Code, insect:Insect}) :-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    Type_atom == queen_bee,
    game:insects:place_insect(Player_id, Type_atom, Hexagon, Insect),
    game:place_queen_bee(Player_id),
    game:increment_number_of_moves(Player_id),
    game:next_player(),
    Status_Code = 200,
    !.
placeInsect(_{type:Type, hexagon:Hexagon}, _{status_code:Status_Code, insect:Insect}) :-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:insects:place_insect(Player_id, Type_atom, Hexagon, Insect),
    game:increment_number_of_moves(Player_id),
    game:next_player(),
    Status_Code = 200.

% move insect
moveInsect(_{type:Type, hexagon_ori:Hexagon_Ori, hexagon_end:Hexagon_End}, _{status_code:Status_Code, msg:MSG}) :-
    (not(compound(Hexagon_Ori)); not(compound(Hexagon_End)); not(atom(Type))),
    Status_Code = 400,
    MSG = "Wrong params!",
    !.
moveInsect(_{type:Type, hexagon_ori:Hexagon_Ori, hexagon_end:Hexagon_End}, _{status_code:Status_Code, msg:MSG}) :-
    string_to_atom(Type, TypeA),
    game:current_player(Player_id),
    game:player(Player_id, _, _, _, _, _),
    game:insects:possible_moves(Player_id, TypeA,_,  Hexagon_Ori, Moves, Status_Code),
    % Player_id, Type_atom, Id, Hexagon, MSG, Status_Code
    not(member(Hexagon_End, Moves)),
    Status_Code = 400,
    MSG = "Wrong placement",
    !.
moveInsect(_{type:Type, id:Id, lvl:Lvl, hexagon_ori:Hexagon_Ori, hexagon_end:Hexagon_End}, _{status_code:Status_Code, insectRes:InsectRes}):-
    string_to_atom(Type, TypeA),
    game:current_player(Player_id),
    game:insects:move_insect(TypeA, Id, Player_id, Lvl, Hexagon_Ori, Hexagon_End, InsectRes),
    game:increment_number_of_moves(Player_id),
    game:next_player(),
    Status_Code = 200.

% Get possible placements
% getPossiblePlacements(_{type:_}, _{status_code:Status_Code, msg:MSG}):-
%     game:current_player(Player_id),
%     game:player(Player_id, Name, Number_of_moves, _, _, _),
%     not(insects:can_play(Player_id, Name, Number_of_moves)),
%     string_concat(Name, " cannot play and needs to pass its turn.", MSG),
%     Status_Code = 401,
%     game:next_player(),!.

getPossiblePlacements(_{type:_}, _{status_code:Status_Code, msg:MSG}):-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, _, _, _),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    Placements == [],
    MSG = "There is unfortunately nowhere where you are allowed to add a new pawn.",
    Status_Code = 400,
    !.
getPossiblePlacements(_{type:Type}, _{status_code:Status_Code, placements:Placements}):-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, Queen_bee_placed, _, _),
    Queen_bee_placed == false,
    Number_of_moves == 3,
    Type_atom == queen_bee,
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    Status_Code = 200,
    !.
getPossiblePlacements(_, _{status_code:Status_Code, msg:MSG}):-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, Queen_bee_placed, _, _),
    Queen_bee_placed == false,
    Number_of_moves == 3,
    Status_Code = 400,
    MSG = "4th move: you have to add your queen!",
    !.
getPossiblePlacements(_, _{status_code:Status_Code, placements:Placements}) :-
    game:current_player(Player_id),
    game:player(Player_id, _, Number_of_moves, _, _, _),
    game:insects:possible_placements(Player_id, Number_of_moves, Placements),
    Status_Code = 200.

% Get possible moves
getPossibleMoves(_{type:Type, id:Id,  hexagon:Hexagon}, _{status_code:Status_Code, msg:MSG}):-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:player(Player_id, _, _, _, _, _),
    game:insects:possible_moves(Player_id, Type_atom, Id, Hexagon, MSG, Status_Code),
    Status_Code == 400,
    !.
getPossibleMoves(_{type:Type, id:Id,  hexagon:Hexagon}, _{status_code:Status_Code, msg:MSG}):-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:player(Player_id, _, _, _, _, _),
    game:insects:possible_moves(Player_id, Type_atom, Id, Hexagon, MSG, Status_Code),
    Status_Code == 400,
    !.
getPossibleMoves(_{type:Type, id:Id, hexagon:Hexagon}, _{status_code:Status_Code, moves:Moves}):-
    string_to_atom(Type, Type_atom),
    game:current_player(Player_id),
    game:player(Player_id, _, _, _, _, _),
    game:insects:possible_moves(Player_id, Type_atom, Id, Hexagon, Moves, Status_Code).

% quee surrounded
queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    not(insects:queen_surrounded(p1)),
    not(insects:queen_surrounded(p2)),
    MSG = "No queen locked.",
    Status_Code = 200,!.
queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    insects:queen_surrounded(p1),
    insects:queen_surrounded(p2),
    MSG = "Both queens locked up.",
    Status_Code = 201,!.
queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    insects:queen_surrounded(p1),
    MSG = "p1's queen blocked.",
    Status_Code = 202,!.
queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    insects:queen_surrounded(p2),
    MSG = "p2's queen blocked.",
    Status_Code = 203,!.

% New Game
newGame(_{mode:Mode, level:Level}, _{msg:MSG}):-
    string_to_atom(Mode, ModeAtom),
    game:new_game(ModeAtom, Level, MSG).

% --------------------------------------Request Handlers--------------------------------------

% Handle place insect
handle_request_place_insect(Req) :-
    http_read_json_dict(Req, Query),
    placeInsect(Query, Res),
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

% handle queen sorrounded
handle_request_queen_surrounded(Req):-
    queenSurrounded(Res),
    reply_json_dict(Res).

% handle new game
handle_request_new_game(Req):-
    http_read_json_dict(Req, Query),
    newGame(Query, Res),
    reply_json_dict(Res).
% --------------------------------------Start server--------------------------------------
start_server(Port) :-
    game:init_game(pvp, 0),%by default mode=pvp, lvl=0(beginner)
    http_server(http_dispatch, [port(Port)]).

% --------------------------------------Initialization--------------------------------------
:- initialization(start_server(3030), program).
