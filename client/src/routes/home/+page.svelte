<script lang="ts">
    import RoomList from '$lib/components/RoomList.svelte';
        
    import type { PageData } from './$types';

    export let data: PageData;  

    $: {
      console.log(JSON.stringify(data, null, 2));
    }
</script>

<div class="flex bg-zinc-9/90 rounded-l-8 overflow-hidden">
    <RoomList data={data} />
    <div class="grid grid-rows-[auto_1fr_auto] p-10 w-full">
      {#if data.rooms.length > 0}
        <div>
          <h2>Sala #{data.rooms[0].roomCode}</h2>
          <div class="text-zinc-5 text-sm">
            {data.rooms[0].users.length} membro{data.rooms[0].users.length > 1 ? 's': ''}, 0 online.
          </div>
        </div>
        <div></div>
        <div class="grid grid-cols-[1fr_auto] gap-2">
          <input
            placeholder="Mensagem"
            type="text"
            class="bg-zinc-8 w-full outline-none border-none p-5 rounded-lg text-zinc-3 text-base">
          <div class="bg-indigo-8 px-5 py-2 rounded-lg grid place-items-center gap-2
            cursor-pointer hover:brightness-90 transition-250 grid-flow-col">
            <span>Enviar</span>
            <span class="i-heroicons:paper-airplane-solid text-2xl" />
          </div>
        </div>
      {/if}
    </div>
</div>
