% Servidor HTTP
:- use_module(library(http/thread_httpd)).
:- use_module(library(http/http_dispatch)).
:- use_module(library(http/http_parameters)).
:- use_module(library(http/http_json)).
:- use_module(library(http/http_header)).

% Declara predicados dinâmicos.
:- dynamic(user/4).
:- dynamic(room/2).
:- dynamic(friend/2).
:- dynamic(post/5).
:- dynamic(session/2).

:- http_handler('/user', handle_user(Method), [method(Method), methods([get,post])]).
:- http_handler('/session', handle_session(Method), [method(Method), methods([get,post])]).
:- http_handler('/login', handle_login, [methods([post])]).

handle_login(Request) :-
    http_read_json_dict(Request, Data),
    (
        check_password(Data.username, Data.password, UserId)
        ->  reply_json_dict(UserId)
        ;   reply_json_dict('Wrong Credentials', [status(401)])
    ).

handle_session(get, Request) :-
    http_parameters(Request, [sessionId(SessionId, [integer])]),
    (
        session(SessionId, UserId)
        -> reply_json_dict(UserId)
        ;  reply_json_dict('Not found', [status(404)])
    ).

handle_session(post, Request) :-
    http_read_json_dict(Request, Data),
    assertz(session(Data.sessionId, Data.id)),
    reply_json_dict('Success').

handle_user(post, Request) :-
    http_read_json_dict(Request, Data),
    (
        user(Data.id, _, _, _)
        ->  reply_json_dict('Already Exists', [status(409)])
        ;   assertz(user(Data.id, Data.username, Data.avatarId, Data.password)),
            reply_json_dict('Success')
    ).

handle_user(get, Request) :-
    http_parameters(Request, [id(Id, [integer])]),
    (
        user(Id, Username, AvatarId, _)
        ->  reply_json_dict(json{ id: Id, username: Username, avatarId: AvatarId })
    ;   reply_json_dict('Not found', [status(404)])
    ).

% Checar se sessão existe.
session_exists(SessionId) :-
    session(SessionId, _).

% Obter a lista de todos os posts.
get_all_posts(Posts) :-
    findall(post(PostId, Title, Description, Date, Author), post(PostId, Title, Description, Date, Author), Posts).

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
    room(RoomCode, UserIds).

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

% Retorna quais salas um usuário está dentro
get_users_rooms(UserId, Rooms) :-
    findall(RoomCode, room(RoomCode, UserIds), Rooms),
    member(UserId, UserIds).

% Adicionar um usuário a uma sala.
add_user_to_room(UserId, RoomCode) :-
    % Checar se a sala existe, e se não, criar uma nova.
    (room(RoomCode, _) -> true; create_room(RoomCode)),
    room(RoomCode, UserIds),
    construct_new_list(UserIds, [UserId], NewUserIds),
    retractall(room(RoomCode, _)),
    assertz(room(RoomCode, NewUserIds)).

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

user(1,a,2,b).

% Iniciar o servidor na porta 6162
:- initialization http_server(http_dispatch, [port(6162)]).
