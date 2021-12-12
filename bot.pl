:- module(bot, 
    [
        play_random/6
    ]).

:-use_module(insects).
:-use_module("./utils/list_utils").


play_random(Player_id, Name, Number_of_moves, Queen_bee_placed, Status_Code, MSG):-
    insects:other_player(Player_id, Other_player_id),
    P1 = Player_id,
    P2 = Other_player_id,


    insects:all_insects(_, _, P1, _, false, _, Non_placed_insects_p1),
    insects:all_insects(_, _, P2, _, false, _, _),
    
    insects:all_insects(_, _, P1, _, true, _, _),
    insects:all_insects(_, _, P2, _, true, _, _),

    analize_type_of_play(P1, Number_of_moves, Type_of_play, Placements, _),
    [L,U] = Type_of_play,
    
    random_between(L, U, Rd),
    
    switch(Rd,
        [
            0: place(P1, Name, Number_of_moves, Non_placed_insects_p1, Placements, Queen_bee_placed, MSG, Status_Code),
            1: place(P1, Name, Number_of_moves, Non_placed_insects_p1, Placements, Queen_bee_placed, MSG, Status_Code),
            -1: (Status_Code=400, MSG="Can't play")
        ]).

switch(X, [Val:Goal|Cases]) :-
    ( X=Val ->
        call(Goal)
    ;
        switch(X, Cases)
    ).

analize_type_of_play(Player_id, Number_of_moves, Type_of_play, Placements, _):-
    not(insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Placements)),
    not(insects:can_move_any_of_the_insects(_,_)),
    Type_of_play = [-1,-1],!.
analize_type_of_play(Player_id, Number_of_moves, Type_of_play, Placements, _):-
    insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Placements),
    not(insects:can_move_any_of_the_insects(_,_)),
    Type_of_play = [0,0],!.
analize_type_of_play(Player_id, Number_of_moves, Type_of_play, Placements, _):-
    not(insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Placements)),
    insects:can_move_any_of_the_insects(_,_),
    Type_of_play = [1,1],!.
analize_type_of_play(Player_id, Number_of_moves, Type_of_play, Placements, _):-
    insects:can_place_any_of_the_insects(Player_id, Number_of_moves, Placements),
    insects:can_move_any_of_the_insects(_,_),
    Type_of_play = [0,1],!.

place(Player_id, Name, Number_of_moves, _, Placements, Queen_bee_placed, MSG, Status_Code):-
    Number_of_moves == 3,
    Queen_bee_placed == false,

    length(Placements, Len_placements),
    random_between(1, Len_placements, Rd_placements),
    
    element_at(Hex, Placements, Rd_placements),
    
    place_insect(Player_id, queen_bee, Hex, Insect),

    string_concat(Name, " places queen_bee", MSG),

    Status_Code = 200,
    !.
place(Player_id, Name, _, Non_placed_insects, Placements, _, MSG, Status_Code):-
    length(Placements, Len_placements),
    random_between(1, Len_placements, Rd_placements),
    
    length(Non_placed_insects, Len_non_placed_insect),
    random_between(1, Len_non_placed_insect, Rd_non_placed_insect),
    
    element_at(Hex, Placements, Rd_placements),
    element_at([Type|_], Non_placed_insects, Rd_non_placed_insect),
    
    place_insect(Player_id, Type, Hex, Insect),

    string_concat(Name, " places ", S1),
    string_concat(S1, Type, MSG),

    Status_Code = 200,
    !.

move():-
    !.
