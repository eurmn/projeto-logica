% Servidor HTTP
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).

% Declara predicados dinâmicos.
:- dynamic(user/4).
:- dynamic(room/3).
:- dynamic(session/2).
:- dynamic(friend/2).
:- dynamic(post/5).
:- dynamic(chat_history/2).

:- http_handler('/user', handle_user(Method), [method(Method), methods([get,post])]).
:- http_handler('/session', handle_session(Method), [method(Method), methods([get,post])]).
:- http_handler('/room', handle_room(Method), [method(Method), methods([get,post])]).
:- http_handler('/room/delete', handle_room_delete(Method), [method(Method), methods([post])]).
:- http_handler('/post', handle_post(Method), [method(Method), methods([post])]).
:- http_handler('/all_posts', handle_all_posts, [methods([get])]).
:- http_handler('/login', handle_login, [methods([post])]).

handle_all_posts(_) :-
    get_all_posts(Posts),
    reply_json_dict(Posts).

get_all_posts(Posts) :-
    findall(json{ id: Id, title: Title, description: Description, date: Date, author: Author },
        post(Id, Title, Description, Date, Author),
    Posts).

handle_post(post, Request) :-
    http_read_json_dict(Request, Data),
    (
        post(Data.id, _, _, _, _)
        ->  edit_post(Data.id, Data.title, Data.description, Data.date, Data.author),
            reply_json_dict('Success')
        ;   create_post(Data.id, Data.title, Data.description, Data.date, Data.author),
            reply_json_dict('Success')
    ).

handle_room(get, Request) :-
    http_parameters(Request, [id(MyUserId, [integer])]),
    findall(json{ roomCode: RoomCode, users: UserInfos },
        (
            room(RoomCode, UserIds, _),
            member(MyUserId, UserIds),
            findall(json{ id: UserId, username: Username, avatarId: AvatarId },
                (
                    member(UserId, UserIds),
                    user(UserId, Username, AvatarId, _)
                ),
            UserInfos)
        ),
    RoomInfos),
    reply_json_dict(RoomInfos).

handle_room(post, Request) :-
    http_read_json_dict(Request, Data),
    assertz(room(Data.roomCode, [Data.owner], Data.owner)),
    reply_json_dict('Success').

handle_room_delete(post, Request) :-
    http_read_json_dict(Request, Data),
    retractall(room(Data.roomCode, _, _)),
    reply_json_dict('Success').

handle_login(Request) :-
    http_read_json_dict(Request, Data),
    (
        check_password(Data.username, Data.password, UserId)
        ->  reply_json_dict(UserId)
        ;   reply_json_dict('Wrong Credentials', [status(401)])
    ).

handle_session(get, Request) :-
    http_parameters(Request, [sessionId(SessionId, [string])]),
    (
        session(SessionId, UserId)
        -> reply_json_dict(UserId)
        ;  reply_json_dict('Not found', [status(404)])
    ).

handle_session(post, Request) :-
    http_read_json_dict(Request, Data),
    retractall(session(Data.sessionId, _)),
    assertz(session(Data.sessionId, Data.id)),
    reply_json_dict('Success').

handle_user(post, Request) :-
    % POST http://localhost:6162/user
    http_read_json_dict(Request, Data),
    (
        user(Data.id, _, _, _)
        ->  reply_json_dict('Already Exists', [status(409)])
        ;   assertz(user(Data.id, Data.username, Data.avatarId, Data.password)),
            reply_json_dict('Created', [status(201)])
    ).

handle_user(get, Request) :-
    % GET http://localhost:6162/user?id=0
    http_parameters(Request, [id(Id, [integer])]),
    (
        user(Id, Username, AvatarId, _)
        ->  reply_json_dict(json{ id: Id, username: Username, avatarId: AvatarId })
    ;   reply_json_dict('Not found', [status(404)])
    ).

% Checar se sessão existe.
session_exists(SessionId) :-
    session(SessionId, _).

% Criar um novo post.
create_post(PostId, Title, Description, Date, Author) :-
    % Apenas criar o post se existe um usuário com o id do autor citado.
    user(Author, _, _, _),
    assertz(post(PostId, Title, Description, Date, Author)).

% Editar um post.
edit_post(PostId, Title, Description, Date, Author) :-
    % Apenas editar o post se o post existir.
    post(PostId, _, _, _, _),
    retractall(post(PostId, _, _, _, _)),
    assertz(post(PostId, Title, Description, Date, Author)).

% Obter um post pelo UserId do autor.
get_posts_by_user_id(UserId, Posts) :-
    findall(post(PostId, Title, Description, Date, Author), post(PostId, Title, Description, Date, Author), Posts),
    member(post(_, _, _, _, UserId), Posts).

% Obter a lista de avatar_ids de um usuário.
get_avatar_id(UserId, AvatarId) :-
    user(UserId, _, AvatarId, _).

% Obter os usuários em uma sala.
get_rooms_users(RoomCode, UserIds) :-
    room(RoomCode, UserIds, _).

% Checar senha
check_password(UserName, PasswordHash, UserId) :-
    user(UserId, UserName, _, PasswordHash).

% Predicado auxiliar para construir novas listas.
construct_new_list([], NewListItems, NewListItems).
construct_new_list([H|T], NewListItems, Result) :-
    construct_new_list(T, NewListItems, TempResult),
    Result = [H|TempResult].

% Encontrar todos os user_ids relacionados a um username.
get_all_users(UserIds) :-
    findall(UserId, user(UserId, _, _, _), UserIds).

% Criar uma nova sala.
create_room(RoomCode) :-
    assertz(room(RoomCode, [])).

% Retorna informações de todas as salas que o usuário pertence.
get_users_rooms(UserId, Rooms, UserNames) :-
    findall(RoomCode, room(RoomCode, UserIds, _), Rooms),
    findall(UserName, user(UserId, UserName, _, _), UserNames),
    member(UserId, UserIds).

% Adicionar um usuário a uma sala.
add_user_to_room(UserId, RoomCode) :-
    % Checar se a sala existe, e se não, criar uma nova.
    (room(RoomCode, _, _) -> true; create_room(RoomCode)),
    room(RoomCode, UserIds, RoomOwner),
    construct_new_list(UserIds, [UserId], NewUserIds),
    retractall(room(RoomCode, _, _)),
    assertz(room(RoomCode, NewUserIds, RoomOwner)).

% Checa se UserId1 e UserId2 são amigos.
is_friend(UserId1, UserId2) :-
    friend(UserId1, UserId2).

% Retorna uma lista de todos os amigos de um usuário.
get_friends(UserId, Friends) :-
    findall(Friend, is_friend(UserId, Friend), Friends).

% Adicionar dois usuários como amigos.
add_friend(UserId1, UserId2) :-
    assertz(friend(UserId1, UserId2)),
    assertz(friend(UserId2, UserId1)).

room(0,[0],0).

% Iniciar o servidor na porta 6162
:- initialization http_server(http_dispatch, [port(6162)]).
