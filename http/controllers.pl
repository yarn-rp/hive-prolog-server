:- module(controllers,[]).

:- use_module(game).


ping_pong(_{status_code:Status_Code, msg:Msg}) :-
    Status_Code = 200,
    Msg = "pong".

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

% getLast(_{hexagon:Hexagon}, _{status_code:Status_Code ,msg:MSG}):-
%     insects:get_last_insect(Hexagon, Insect),
%     Insect == [],
%     MSG = "Empty hexagon!",
%     Status_Code = 400,!.
% getLast(_{hexagon:Hexagon}, _{status_code:Status_Code ,insect:Insect}):-
%     insects:get_last_insect(Hexagon, Insect),
%     Status_Code = 200.

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

queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    not(insects:queen_surrounded(p1)),
    not(insects:queen_surrounded(p2)),
    MSG = "No queen locked.",
    Status_Code = 200,!.
queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    insects:queen_surrounded(p1),
    insects:queen_surrounded(p2),
    MSG = "Draw!",
    Status_Code = 201,!.
queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    insects:queen_surrounded(p1),
    game:player(p2, Name, _, _, _, _),
    string_concat(Name, " won!", MSG),
    Status_Code = 202,!.
queenSurrounded(_{status_code:Status_Code, msg:MSG}):-
    insects:queen_surrounded(p2),
    game:player(p1, Name, _, _, _, _),
    string_concat(Name, " won!", MSG),
    Status_Code = 203,!.


playAI(_{status_code:Status_Code, msg:MSG}):-
    game:player(Id, _, Number_of_moves, Queen_bee_placed, ai, false),
    game:play_ai(Id, Number_of_moves, Queen_bee_placed, Status_Code, MSG),
    Status_Code == 200,
    game:increment_number_of_moves(Id),
    game:next_player(),!.
playAI(_{status_code:Status_Code, msg:MSG}):-
    game:player(Id, _, Number_of_moves, Queen_bee_placed, ai, false),
    game:play_ai(Id, Number_of_moves, Queen_bee_placed, Status_Code, MSG),
    game:next_player().

newGame(_{mode:Mode, level:Level}, _{msg:MSG}):-
    string_to_atom(Mode, ModeAtom),
    game:new_game(ModeAtom, Level, MSG).
