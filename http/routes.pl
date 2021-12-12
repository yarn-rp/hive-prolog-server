:- module(routes,[start_server/1]).

:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_json)).

:- use_module("../game").
:- use_module("./controllers").


:- http_handler('/insect/place/available', handlePlacesAvailable, []).
:- http_handler('/insect/move/available', handleMovesAvailable, []).
:- http_handler('/insect/place', handle_request_place_insect, []).
:- http_handler('/insect/move', handleMove, []).
:- http_handler('/game/arena', handleArena, []).
:- http_handler('/game', handleNewGame, []).
:- http_handler('/ai', handleAiPlay, []).


handle_request_place_insect(Req) :-
    http_read_json_dict(Req, Query),
    controllers:placeInsect(Query, Res),
    reply_json_dict(Res).

handleMove(Req):-
    http_read_json_dict(Req, Query),
    controllers:moveInsect(Query, Res),
    reply_json_dict(Res).

handlePlacesAvailable(Req) :-
    http_read_json_dict(Req, Query),
    controllers:getPossiblePlacements(Query, Placements),
    Res = Placements,
    reply_json_dict(Res).

handleMovesAvailable(Req):-
    http_read_json_dict(Req, Query),
    controllers:getPossibleMoves(Query, Res),
    reply_json_dict(Res).

handleArena(_):-
    game:current_player(Current_player_id),
    game:player(p1, P1Name, P1MovesCount, P1HasQueenOnArena, P1Type, P1GameOver),
    game:player(p2, P2Name, P2MovesCount, P2HasQueenOnArena, P2Type, P2GameOver),

    game:insects:allInsects(_, _, p1, _, false,_, P1Hand),
    game:insects:allInsects(_, _, p2, _, false,_, P2Hand),

    game:insects:allInsects(_, _, _, _, true,_,ArenaInsects),
    

    Players_info =
    _{
        p1:_{
            id:p1, name:P1Name,    
            movesCount:P1MovesCount,
            hasQueenOnArena:P1HasQueenOnArena,
            hand: P1Hand,
            type: P1Type,
            isGameOver: P1GameOver
            },
        p2:_{
            id:p2, name:P2Name,
            movesCount:P2MovesCount,
            hasQueenOnArena:P2HasQueenOnArena,
            hand: P2Hand,
            type: P2Type,
            isGameOver: P2GameOver
            }
    },

    InsectsOnArena = ArenaInsects,

    Status_Code = 200,

    Res = _{status_code:Status_Code, current_player_id:Current_player_id, players_info:Players_info, hive:InsectsOnArena},
    reply_json_dict(Res).

handle_request_queen_surrounded(_):-
    controllers:queenSurrounded(Res),
    reply_json_dict(Res).

handleAiPlay(_):-
    controllers:playAI(Res),
    reply_json_dict(Res).

handleNewGame(Req):-
    http_read_json_dict(Req, Query),
    controllers:newGame(Query, Res),
    reply_json_dict(Res).


start_server(Port) :-
    game:initialize(pvp, 0),%by default mode=pvp, lvl=0(beginner)
    http_server(http_dispatch, [port(Port)]).


:- initialization(start_server(3031), program).
