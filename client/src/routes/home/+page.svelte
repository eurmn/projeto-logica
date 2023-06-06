<script lang="ts">
  import Dash from "$lib/components/Dash.svelte";
  import Post from "$lib/components/Post.svelte";
  import RoomList from "$lib/components/RoomList.svelte";
  import type { Room } from "$lib/types";

  import type { PageData } from "./$types";

  export let data: PageData;

  let selectedRoom: Room | undefined;

  async function deleteRoom(roomCode: string | undefined) {
    if (!roomCode) return;

    let res = await fetch("/api/rooms", {
      method: "DELETE",
      headers: {
        "Content-Type": "application/json",
      },
      body: JSON.stringify({
        roomCode,
      }),
    });

    console.log(await res.text());

    if (res.ok) {
      data.rooms = data.rooms.filter((r) => r.roomCode !== roomCode);
    }
  }

  $: {
    selectedRoom = data.rooms.length > 0 ? data.rooms[0] : undefined;
  }
</script>

<div class="flex bg-zinc-9/90 rounded-l-8 overflow-hidden">
    <RoomList
      {data}
      onRoomSelected={(r) => (selectedRoom = r)}
      onCreateRoom={(r) =>
        (data.rooms = [
          {
            roomCode: r,
            users: [{ id: data.userData.id, username: data.userData.username }],
            owner: data.userData.id,
          },
          ...data.rooms,
        ])}
    />
    <div class="grid grid-rows-[auto_1fr_auto] p-10 w-full">
      {#if selectedRoom}
        <div class="flex justify-between">
          <div>
            <h2>
              Sala {selectedRoom.roomCode.slice(0, 3) +
                "-" +
                selectedRoom.roomCode.slice(3)}
            </h2>
            <div class="text-zinc-5 text-sm">
              {selectedRoom.users.length} membro{selectedRoom.users.length > 1
                ? "s"
                : ""}, 0 online.
            </div>
          </div>
          <div class="text-zinc-4 h-full flex items-center gap-4">
            <div
              class="i-heroicons:magnifying-glass-20-solid text-3xl cursor-pointer
                  hover:brightness-80 transition-250"
            />
            <div
              class="i-heroicons:user-plus-solid text-3xl cursor-pointer
                  hover:brightness-80 transition-250"
            />
            <!-- svelte-ignore a11y-click-events-have-key-events -->
            <div
              on:click={() => deleteRoom(selectedRoom?.roomCode)}
              class="i-heroicons:trash-solid text-3xl cursor-pointer
                    hover:brightness-80 transition-250"
            />
          </div>
        </div>
        <div />
        <div class="grid grid-cols-[1fr_auto] gap-2">
          <input
            placeholder="Mensagem"
            type="text"
            class="bg-zinc-8 w-full outline-none border-none p-5 rounded-lg text-zinc-3 text-base"
          />
          <div
            class="bg-indigo-8 px-5 py-2 rounded-lg grid place-items-center gap-2
              cursor-pointer hover:brightness-90 transition-250 grid-flow-col"
          >
            <span>Enviar</span>
            <span class="i-heroicons:paper-airplane-solid text-2xl" />
          </div>
        </div>
      {/if}
    </div>
</div>
