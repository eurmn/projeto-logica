<script lang="ts">
    import type { Post } from "$lib/types";
    import { Avatars } from "$lib/utils";
    import { scale } from "svelte/transition";

    export let data: Post;
</script>

<div
    in:scale
    class="rounded-xl bg-white/5 p-5 gap-5 grid grid-flow-col grid-cols-[auto_1fr]"
>
    <div class="flex flex-col gap-1 items-center">
        <img
            src={Avatars[data.author.avatarId].src}
            alt=""
            class="rounded-full"
            width={70}
            height={70}
        />
        <div class="text-xs text-zinc-4">
            {data.author.username}#{data.author.id}
        </div>
    </div>
    <div class="flex flex-col gap-1 overflow-x-hidden">
        <h2>{data.title}</h2>
        <hr class="border-none border-b-1 border-b-solid border-zinc-5 my-1" />
        <!-- word wrap -->
        <div class="text-sm text-zinc-3 p-1
            whitespace-pre-wrap break-words overflow-x-hidden
        ">{data.description}</div>
        <hr class="border-none border-b-1 border-b-solid border-zinc-5 my-1" />
        <span class="ml-auto text-xs text-zinc-4">
            {new Date(data.date).toLocaleDateString("pt-BR")}
        </span>
    </div>
</div>
