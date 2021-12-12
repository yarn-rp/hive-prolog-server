:- module(game,
    [player/6,
    current_player/1,
    initialize/2,
    startNewGame/3, 
    togglePlayer/0,
    incrementMovesCount/1,
    hasQueenPlaced/1,
    AiPlay/5
    ]).

:-use_module(insects).
:-use_module("./bot").

:-dynamic player/6, current_player/1, mode/1, level/1.

initialize(Mode, Level):-
    Mode == pvp,
    assert(mode(Mode)),
    assert(level(Level)),
    assert(player(p1, 'Player 1', 0, false, human, false)),
    assert(player(p2, 'Player 2', 0, false, human, false)),
    assert(current_player(p1)),
    insects:initInsects(),!.
initialize(Mode, Level):-
    Mode == pvai,
    assert(mode(Mode)),
    assert(level(Level)),
    assert(player(p1, 'Player 1', 0, false, human, false)),
    assert(player(p2, 'AI', 0, false, ai, false)),
    assert(current_player(p1)),
    insects:initInsects().

incrementMovesCount(Player_id):-
    player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over),
    Number_of_moves1 is Number_of_moves + 1,
    retract(player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over)),
    assert(player(Player_id, Name, Number_of_moves1, Queen_bee_placed, Type_player, Game_over)).


startNewGame(Mode, Level, MSG):-
    retractall(mode(_)),
    retractall(level(_)),
    retractall(player(_,_,_,_,_,_)),
    retractall(current_player(_)),
    retractall(insects:insect(_,_,_,_,_,_)),
    initialize(Mode, Level),
    MSG = "ok".

togglePlayer():-
    current_player(P),
    P == p1,
    retract(current_player(p1)),
    assert(current_player(p2)),!.
togglePlayer():-
    current_player(P),
    P == p2,
    retract(current_player(p2)),
    assert(current_player(p1)).

hasQueenPlaced(Player_id):-
    player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over),
    retract(player(Player_id, Name, Number_of_moves, Queen_bee_placed, Type_player, Game_over)),
    assert(player(Player_id, Name, Number_of_moves, true, Type_player, Game_over)).

aiPlay(AI_id, Number_of_moves, Queen_bee_placed, Status_Code, MSG):-
    player(AI_id, Name, Number_of_moves, Queen_bee_placed, ai, _),
    play_random(AI_id, Name, Number_of_moves, Queen_bee_placed, Status_Code, MSG).
