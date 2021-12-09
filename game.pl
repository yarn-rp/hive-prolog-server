% ################################################################################
% --------------------------------------GAME--------------------------------------
% ################################################################################

% --------------------------------------EXPORTS--------------------------------------
:- module(game,
    [player/6,
    current_player/1,
    init_game/2,
    reset_game/1, 
    new_game/3, 
    next_player/0,
    increment_number_of_moves/1,
    place_queen_bee/1
    ]).

% --------------------------------------MODULES--------------------------------------
:-consult(insects), import(insects).

% --------------------------------------DYNAMICS--------------------------------------
:-dynamic player/6, current_player/1, mode/1, level/1.
% player(Id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over) | Player Info | p1 is the first player and p2 is the second
% current_player(Id) | The current player' is

% --------------------------------------METHODS--------------------------------------
% init game
init_game(Mode, Level):-
    Mode == pvp,
    assert(mode(Mode)),
    assert(level(Level)),
    assert(player(p1, 'Player 1', 0, false, human, false)),
    assert(player(p2, 'Player 2', 0, false, human, false)),
    assert(current_player(p1)),
    insects:init_insects(),!.
init_game(Mode, Level):-
    Mode == pvai,
    assert(mode(Mode)),
    assert(level(Level)),
    assert(player(p1, 'Player 1', 0, false, human, false)),
    assert(player(p2, 'AI', 0, false, ai, false)),
    assert(current_player(p1)),
    insects:init_insects().

% increment number of moves(Player_id):-
increment_number_of_moves(Player_id):-
    player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over),
    Number_of_moves1 is Number_of_moves + 1,
    retract(player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over)),
    assert(player(Player_id, Name, Number_of_moves1, Queen_bee_placed, Type_player, Game_over)).

% reset game
reset_game(MSG):-
    mode(Mode),
    level(Level),
    retractall(mode(_)),
    retractall(level(_)),
    retractall(player(_,_,_,_,_,_)),
    retractall(current_player(_)),
    retractall(insects:insect(_,_,_,_,_,_)),
    init_game(Mode, Level),
    MSG = "ok".

% new game
new_game(Mode, Level, MSG):-
    retractall(mode(_)),
    retractall(level(_)),
    retractall(player(_,_,_,_,_,_)),
    retractall(current_player(_)),
    retractall(insects:insect(_,_,_,_,_,_)),
    init_game(Mode, Level),
    MSG = "ok".

% next player
next_player():-
    current_player(P),
    P == p1,
    retract(current_player(p1)),
    assert(current_player(p2)),!.
next_player():-
    current_player(P),
    P == p2,
    retract(current_player(p2)),
    assert(current_player(p1)).

% place queen bee 
place_queen_bee(Player_id):-
    player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over),
    retract(player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over)),
    assert(player(Player_id, Name, Number_of_moves, true, Type_player, Game_over)).