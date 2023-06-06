export type UserData = {
    id: number;
    avatarId: number;
    username: string;
}

export type Room = {
    owner: number;
    roomCode: string;
    users: {
        id: number,
        username: string
    }[];
}

export type Post = {
    id: string;
    title: string;
    description: string;
    date: string;
    author: UserData;
}