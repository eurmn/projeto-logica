% Declara predicados dinâmicos.
:- dynamic avatar_id/2.
:- dynamic username/2.
:- dynamic room/2.
:- dynamic friend/2.

% Declara relação dos posts (possuem título, descrição, data de postagem, autor e ID).
:- dynamic post/5.

% Predicado para obter a lista de todos os posts.
get_all_posts(Posts) :-
    findall(post(PostId, Title, Description, Date, Author), post(PostId, Title, Description, Date, Author), Posts).

% Predicado para criar um novo post.
create_post(PostId, Title, Description, Date, Author) :-
    % Apenas criar o post se existe um usuário com o id do autor citado.
    username(Author, _),
    assertz(post(PostId, Title, Description, Date, Author)).

% Predicado para editar um post.
edit_post(PostId, Title, Description, Date, Author) :-
    % Apenas editar o post se o post existir.
    post(PostId, _, _, _, _),
    retractall(post(PostId, _, _, _, _)),
    assertz(post(PostId, Title, Description, Date, Author)).

% Predicado para obter um post pelo UserId do autor.
get_posts_by_user_id(UserId, Posts) :-
    findall(post(PostId, Title, Description, Date, Author), post(PostId, Title, Description, Date, Author), Posts),
    member(post(_, _, _, _, UserId), Posts).

% Predicado para obter a lista de avatar_ids de um usuário.
get_avatar_id(UserId, AvatarId) :-
    avatar_id(UserId, AvatarId).

% Predicado para obter o username de um usuário.
get_username(UserId, UserName) :-
    username(UserId, UserName).

% Predicado para obter os usuários em uma sala.
get_rooms_users(RoomCode, UserIds) :-
    room(RoomCode, UserIds).

% Predicados para criar um novo usuário.
create_user(UserId, UserName) :-
    assertz(username(UserId, UserName)).
create_user(UserId, UserName, AvatarId) :-
    assertz(avatar_id(UserId, AvatarId)),
    assertz(username(UserId, UserName)).

% Predicado auxiliar para construir novas listas.
construct_new_list([], NewListItems, NewListItems).
construct_new_list([H|T], NewListItems, Result) :-
    construct_new_list(T, NewListItems, TempResult),
    Result = [H|TempResult].

% Predicado para encontrar todos os user_ids relacionados a um username.
get_all_users(UserIds) :-
    findall(UserId, username(UserId, _), UserIds).

% Predicado para criar uma nova sala.
create_room(RoomCode) :-
    assertz(room(RoomCode, [])).

% Predicado que retorna quais salas um usuário está dentro
get_users_rooms(UserId, Rooms) :-
    findall(RoomCode, room(RoomCode, UserIds), Rooms),
    member(UserId, UserIds).

% Predicado para adicionar um usuário a uma sala.
add_user_to_room(UserId, RoomCode) :-
    % Checar se a sala existe, e se não, criar uma nova.
    (room(RoomCode, _) -> true; create_room(RoomCode)),
    room(RoomCode, UserIds),
    construct_new_list(UserIds, [UserId], NewUserIds),
    retractall(room(RoomCode, _)),
    assertz(room(RoomCode, NewUserIds)).

% Regra: is_friend(UserId1, UserId2) checa se UserId1 e UserId2 são amigos.
is_friend(UserId1, UserId2) :-
    friend(UserId1, UserId2).

% Regra: get_friends(UserId, Friends) retorna uma lista de todos os amigos de um usuário.
get_friends(UserId, Friends) :-
    findall(Friend, is_friend(UserId, Friend), Friends).

% Predicado para adicionar dois usuários como amigos.
add_friend(UserId1, UserId2) :-
    assertz(friend(UserId1, UserId2)),
    assertz(friend(UserId2, UserId1)).

