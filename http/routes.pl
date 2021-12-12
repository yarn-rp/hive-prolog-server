:- module(routes,[start_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- use_module("../game").
:- use_module("./controllers").

:- http_handler('/hive_api/ping', handle_request_ping_pong, []).
:- http_handler('/hive_api/insect/get_possible_placements', handle_request_get_possible_placements, []).
:- http_handler('/hive_api/insect/get_possible_moves', handle_request_get_possible_moves, []).
:- http_handler('/hive_api/insect/place_insect', handle_request_place_insect, []).
:- http_handler('/hive_api/insect/move_insect', handle_request_move_insect, []).
% :- http_handler('/hive_api/insect/get_last', handle_request_get_last, []).
:- http_handler('/hive_api/game/game_stats', handle_request_game_stats, []).
:- http_handler('/hive_api/game/reset_game', handle_request_reset_game, []).
:- http_handler('/hive_api/game/new_game', handle_request_new_game, []).
:- http_handler('/hive_api/insect/queen_surrounded', handle_request_queen_surrounded, []).
:- http_handler('/hive_api/ai/play', handle_request_play_ai, []).


handle_request_ping_pong(_) :-
    controllers:ping_pong(Res),
    reply_json_dict(Res).

handle_request_place_insect(Req) :-
    http_read_json_dict(Req, Query),
    controllers:placeInsect(Query, Res),
    reply_json_dict(Res).

handle_request_move_insect(Req):-
    http_read_json_dict(Req, Query),
    controllers:moveInsect(Query, Res),
    reply_json_dict(Res).

handle_request_get_possible_placements(Req) :-
    http_read_json_dict(Req, Query),
    controllers:getPossiblePlacements(Query, Placements),
    Res = Placements,
    reply_json_dict(Res).

handle_request_get_possible_moves(Req):-
    http_read_json_dict(Req, Query),
    controllers:getPossibleMoves(Query, Res),
    reply_json_dict(Res).

% handle_request_get_last(Req):-
%     http_read_json_dict(Req, Query),
%     controllers:getLast(Query, Res),
%     reply_json_dict(Res).

handle_request_game_stats(_):-
    game:current_player(Current_player_id),
    game:player(p1, Name_p1, Number_of_moves_p1, Queen_bee_placed_p1, Type_player1, Game_over1),
    game:player(p2, Name_p2, Number_of_moves_p2, Queen_bee_placed_p2, Type_player2, Game_over2),

    game:insects:all_insects(_, _, p1, _, false,_, Non_placed_insects_p1),
    game:insects:all_insects(_, _, p2, _, false,_, Non_placed_insects_p2),

    game:insects:all_insects(_, _, _, _, true,_,Placed_insects),
    

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

handle_request_queen_surrounded(_):-
    controllers:queenSurrounded(Res),
    reply_json_dict(Res).

handle_request_play_ai(_):-
    controllers:playAI(Res),
    reply_json_dict(Res).

handle_request_reset_game(_):-
    game:reset_game(MSG),
    Res = _{msg:MSG},
    reply_json_dict(Res).

handle_request_new_game(Req):-
    http_read_json_dict(Req, Query),
    controllers:newGame(Query, Res),
    reply_json_dict(Res).


start_server(Port) :-
    game:init_game(pvp, 0),%by default mode=pvp, lvl=0(beginner)
    http_server(http_dispatch, [port(Port)]).


:- initialization(start_server(3031), program).
