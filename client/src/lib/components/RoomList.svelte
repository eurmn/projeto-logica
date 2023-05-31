<script lang="ts">
    import type { Room } from '$lib/types';
    import { Avatars } from '$lib/utils';
    import type { PageData } from '../../routes/home/$types';
	import { scale } from 'svelte/transition';

    export let data: PageData;
    export let onCreateRoom: (room: string) => void;
    export let onRoomSelected: (room: Room) => void;

    async function createRoom() {
      let res = await fetch('/api/rooms', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({
          userId: data.userData.id,
        }),
      });

      let newRoomCode = await res.json() as string;
      onCreateRoom(newRoomCode);
    }
</script>

<div
    class="p-10 font-title max-w-lg w-full overflow-hidden flex flex-col gap-6"
>
    <div
        class="gap-3 inline-flex items-center py-4 px-6 w-full bg-indigo-8
        rounded-xl my-2 justify-start overflow-hidden min-h-24"
    >
        <img
            src={Avatars[data.userData.avatarId].src}
            alt={Avatars[data.userData.avatarId].alt}
            class="border-black/40 border-solid border-2 w-20 h-20 rounded-full object-cover"
        />
        <div>
            <h3 class="text-white break-words font-normal">
                Bem-vindo, <span class="font-bold"
                    >{data.userData.username}</span
                >.
            </h3>
            <div class="text-zinc-4 text-sm">
                {data.userData.username}#{data.userData.id}
            </div>
        </div>
    </div>
    <div class="flex gap-3 flex-col w-full">
        <div class="mx-4 rounded-xl text-zinc-5 gap-2 flex items-center mb-4">
            <span class="text-3xl i-heroicons-magnifying-glass-solid" />
            <input
                placeholder="Search"
                type="text"
                class="top-0 left-0 bg-transparent p-4 rounded-xl border-none bg-white/5
                focus:outline-none w-full placeholder:text-zinc-5 text-zinc-3"
            />
        </div>
        <!-- svelte-ignore a11y-click-events-have-key-events -->
        <div
            on:click={() => createRoom()}
            class="w-full bg-white/5 p-3 rounded-xl flex gap-3
                cursor-pointer hover:brightness-90 transition-250"
        >
            <div
                class="cursor-pointer h-15 w-15 bg-zinc-7 rounded-xl grid place-items-center"
            >
                <span class="i-heroicons:plus-circle-20-solid text-4xl" />
            </div>
            <div class="flex flex-col justify-evenly">
                <div class="text-sm">Nova Sala</div>
                <div class="text-xs text-zinc-4">
                    Crie uma nova sala de bate-papo.
                </div>
            </div>
        </div>
    </div>
    <!-- <div class="h-0 w-full border-b-1 border-b-solid border-zinc-5"></div> -->
    <div class="flex gap-3 flex-col h-full w-full overflow-y-auto">
        {#if data.rooms.length > 0}
            {#each data.rooms as room (room.roomCode)}
                <!-- svelte-ignore a11y-click-events-have-key-events -->
                <div
                    
                    transition:scale
                    on:click={() => onRoomSelected(room)}
                    class="w-full bg-white/5 p-3 rounded-xl flex gap-3
                        cursor-pointer hover:brightness-90 transition-250"
                >
                    <div
                        class="cursor-pointer h-15 w-15 bg-zinc-7 rounded-xl grid place-items-center"
                    >
                        <span class="i-heroicons:user-group-solid text-4xl" />
                    </div>
                    <div class="flex flex-col justify-evenly">
                        <div class="text-sm">Sala {
                            room.roomCode.slice(0, 3) + '-' + room.roomCode.slice(3)
                        }</div>
                        <div class="text-xs text-zinc-4">
                            {room.users.map(u => u.id === data.userData.id ? 'Você' : u.username).join(', ')}.
                        </div>
                    </div>
                </div>
            {/each}
        {/if}
    </div>
</div>
