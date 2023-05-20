export type UserData = {
    id: number;
    avatarId: number;
    username: string;
}

export type Room = {
    owner: number;
    roomCode: number;
    users: {
        id: number,
        username: string
    }[];
}