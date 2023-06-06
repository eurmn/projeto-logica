<script lang="ts">
    import { goto } from "$app/navigation";
    import { base } from "$app/paths";
</script>

<div
    class="sm:flex display-none bg-none px-7 py-10 h-full text-zinc-3 grid gap-8 flex-col font-[Roboto]"
>
    <span class="text-xl mb-7 font-title font-extrabold">Social</span>
    <!-- svelte-ignore a11y-click-events-have-key-events -->
    <a
        href="/"
        class="flex flex-col gap-0.5 justify-center cursor-pointer
        hover:brightness-75 transition-250"
    >
        <span
            class="i-heroicons-chat-bubble-left-right-solid text-4xl mx-auto"
        />
        <span class="text-sm text-center">chats</span>
    </a>
    <!-- svelte-ignore a11y-click-events-have-key-events -->
    <a
        href="/home/posts"
        class="flex flex-col gap-0.5 justify-center cursor-pointer
        hover:brightness-75 transition-250"
    >
        <span class="i-heroicons-pencil-square-20-solid text-4xl mx-auto" />
        <span class="text-sm text-center">posts</span>
    </a>
    <!-- svelte-ignore a11y-click-events-have-key-events -->
    <div
        class="flex flex-col gap-0.5 justify-center cursor-pointer
    hover:brightness-75 transition-250 mt-auto"
        on:click={() => goto(`/login?signout=1`)}
    >
        <span class="i-heroicons:arrow-left-circle-20-solid text-4xl mx-auto" />
        <span class="text-sm text-center">sign out</span>
    </div>
</div>
