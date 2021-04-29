:- use_module(library(http/websocket)).
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/hub)).
:- use_module(library(http/json)).
:- use_module(library(http/json_convert)).


:- dynamic(visitor/1).
:- dynamic(field/3).
:- dynamic(game_id/2).
:- dynamic(pair/2).
:- dynamic(field_size/2).
:- dynamic(ready/2).
:- dynamic(shipIdCnt/4).
:- dynamic(fieldToShip/4).
:- dynamic(totalShips/3).
:- dynamic(idToCoods/3).

place_ships(Id, Client, Coords, ShipId) :-
    atom_number(ShipIdA, ShipId),
    Ship = Coords.get(ShipIdA), !,
    _{size:Size, arr:Arr} :< Ship,

    assertz(shipIdCnt(Id, Client, ShipId, Size)),
    totalShips(Id, Client, Total),
    retract(totalShips(Id, Client, Total)),
    TotalN is Total+Size,
    assertz(totalShips(Id, Client, TotalN)),

    place_ship(Id, Client, Arr, ShipId),
    ShipIdN is ShipId + 1,
    place_ships(Id, Client, Coords, ShipIdN).

place_ships(Id, Client, Coords, ShipId).

place_ship(Id, Client, [Current | OtherShips], ShipId) :-
    assertz(field(Id, Client, Current)),
    assertz(fieldToShip(Id, Client, Current, ShipId)),
    place_ship(Id, Client, OtherShips, ShipId).

place_ship(Id, Client, [], ShipId).

:- http_handler(root(test),
    http_upgrade_to_websocket(accept_socket, 
    [guarded(false), subprotocols([chat])]),
    [id(battleships)]).

accept_socket(WebSocket) :-
    hub_add(main, WebSocket, _Id).

handle_message(Message, Hub) :- 
	websocket{client:Client,data:Data,format:string,hub:main,opcode:text} :< Message, !,
	atom_string(Data1,Data),
	json:atom_json_dict(Data1, Json, []),
	handle_json_message(Json, Client, Hub).

handle_message(Message, _Room) :-
	hub{joined:Id} :< Message, !,
	assertz(visitor(Id)).

handle_message(Message, _Room) :-
	hub{left:Id} :< Message, !.

handle_json_message(_{status:"game_started", size:Size, id:Id}, Client, Hub) :-
    assertz(game_id(Id, Client)),
    assertz(field_size(Id, Size)),
    hub_send(Client, json(json([status='game_started', size=Size, id=Id, message='Waiting for player 2.']))).

handle_json_message(_{status:"join", id:Id}, Client, Hub) :-
    assertz(game_id(Id, Client)),
    field_size(Id, Size),
    aggregate_all(count, game_id(Id, X), Count),
    (Count >= 2 -> 
        hub_broadcast(Hub.name, json(json([status='placing', size=Size, id=Id]))),
        assertz(totalShips(Id, Client, 0)),
        game_id(Id, PlayerTwo),
        PlayerTwo \= Client,
        assertz(totalShips(Id, PlayerTwo, 0))
        ;
        hub_send(Client, json(json([status='not_found', message='Room with id not found'])))).

handle_json_message(_{status:"placed", coords:Coords}, Client, Hub) :-
    game_id(Id, Client),
    place_ships(Id, Client, Coords, 1),
    assertz(ready(Id, Client)),
    assertz(idToCoods(Id, Client, Coords)),
    aggregate_all(count, ready(Id, X), Count),
    (Count >= 2 -> 
        hub_send(Client, json(json([status='finished', coords=Coords]))),
        hub_send(Client, json(json([status='wait']))),

        game_id(Id, PlayerTwo),
        PlayerTwo \= Client,
        idToCoods(Id, PlayerTwo, Coords2),

        hub_send(PlayerTwo, json(json([status='finished', coords=Coords2]))),
        hub_send(PlayerTwo, json(json([status='ready'])))
        ;
        hub_send(Client, json(json([status='accepted', message='Waiting for player 2'])))).

handle_json_message(_{status:"turn", coords: Coords}, Client, Hub) :-
    game_id(Id, Client),
	hub_send(Client, json(json([status='not_found', message='1']))),
    game_id(Id, PlayerTwo),
%	hub_send(Client, json(json([status='not_found', message='2']))),
    Client \= PlayerTwo,
	hub_send(Client, json(json([status='not_found', message='3']))),
    fieldToShip(Id, PlayerTwo, Coords, ShipId),
	hub_send(Client, json(json([status='not_found', message='4']))),
    shipIdCnt(Id, PlayerTwo, ShipId, Cnt),
	hub_send(Client, json(json([status='not_found', message='5']))),
    (Cnt >= 2 ->
        hub_send(Client, json(json([status='wait', result='hit', coords=Coords]))),
		hub_send(Client, json(json([status='not_found', message='6']))),
        retract(shipIdCnt(Id, PlayerTwo, ShipId, Cnt)),
		hub_send(Client, json(json([status='not_found', message='7']))),
        CntN is Cnt-1,
		hub_send(Client, json(json([status='not_found', message='8']))),
        assertz(shipIdCnt(Id, PlayerTwo, ShipId, CntN))
        ;
        hub_send(Client, json(json([status='wait', result='kill', coords=Coords]))),
		hub_send(Client, json(json([status='not_found', message='9']))),
        retract(shipIdCnt(Id, PlayerTwo, ShipId, Cnt)),
		hub_send(Client, json(json([status='not_found', message='10']))),
		hub_send(Client, json(json([status='not_found', message='11'])))),
    totalShips(Id, PlayerTwo, Total),
	hub_send(Client, json(json([status='not_found', message='12']))),
    retract(totalShips(Id, PlayerTwo, Total)),
	hub_send(Client, json(json([status='not_found', message='13']))),
    TotalN is Total-1,
	hub_send(Client, json(json([status='not_found', message='14']))),
    assertz(totalShips(Id, PlayerTwo, TotalN)),
	hub_send(Client, json(json([status='not_found', message='15']))),
    (TotalN = 0 ->
        hub_send(Client, json(json([status='victory']))),
        hub_send(PlayerTwo, json(json([status='lost'])))
        ;
        hub_send(Client, json(json([status='ready']))),
		hub_send(PlayerTwo, json(json([status='wait', result='hit', coords=Coords])))).
    

handle_json_message(_{status:"turn", coords: Coords}, Client, Hub) :-
    game_id(Id, Client),
    game_id(Id, PlayerTwo),
    Client \= PlayerTwo,
    hub_send(Client, json(json([status='wait', result='miss', coords=Coords]))),
    hub_send(PlayerTwo, json(json([status='ready']))).

handle_json_message(Json, Client, Hub) :-
    hub_broadcast(Hub, {result: "unrecognized request"}).

listen_sockets(Hub) :-
    thread_get_message(Hub.queues.event, Message),
    handle_message(Message, Hub),
    listen_sockets(Hub).

create_room :-
    hub_create(main, Hub, []),
    thread_create(listen_sockets(Hub), _,
                [ alias(listen_sockets) ]).

start_server :-
    b_setval(counter, 0),
    create_room,
	http_server(http_dispatch, [port(8083)]).