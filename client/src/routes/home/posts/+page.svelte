<script lang="ts">
    import { base } from "$app/paths";
    import Post from "$lib/components/Post.svelte";
    import { Avatars } from "$lib/utils";
    import type { PageData } from "./$types";

    export let data: PageData;

    let title: string;
    let description: string;

    async function createPost() {
        if (!title || !description) return;

        let res: { id: string; date: string } = await (
            await fetch("/api/posts", {
                method: "POST",
                headers: {
                    "Content-Type": "application/json",
                },
                body: JSON.stringify({
                    title,
                    description,
                    userId: data.userData.id,
                }),
            })
        ).json();

        data.posts = [
            {
                ...res,
                title,
                description,
                author: {
                    id: data.userData.id,
                    username: data.userData.username,
                    avatarId: data.userData.avatarId,
                },
            },
            ...data.posts,
        ];
    }
</script>

<div class="flex bg-zinc-9/90 sm:rounded-l-8 p-2 sm:p-7 w-full">
    <div class="sm:max-w-3xl h-full w-full mx-auto flex flex-col gap-2">
        <div
            class="rounded-xl bg-white/5 p-5 sm:gap-5 grid grid-rows-[auto_1fr] sm:grid-cols-[auto_1fr]"
        >
            <div class="flex flex-col gap-1 items-center py-2">
                <img
                    src={Avatars[data.userData.avatarId].src}
                    alt=""
                    class="rounded-full"
                    width={70}
                    height={70}
                />
                <div class="text-xs text-zinc-4">
                    {data.userData.username}#{data.userData.id}
                </div>
            </div>
            <div class="flex flex-col gap-2">
                <div>
                    <div class="mb-1 text-lg font-semibold">Título:</div>
                    <input
                        bind:value={title}
                        type="text"
                        class="color-white w-full outline-none border-none bg-white/5 p-3 rounded-xl"
                    />
                </div>
                <div>
                    <div class="mb-1 text-lg font-semibold">Conteúdo:</div>
                    <textarea
                        bind:value={description}
                        class="min-h-30 resize-none color-white w-full outline-none border-none
                        bg-white/5 p-3 rounded-xl"
                    />
                </div>
                <!-- svelte-ignore a11y-click-events-have-key-events -->
                <span
                    on:click={() => createPost()}
                    class="ml-auto bg-indigo-8 rounded-lg py-2 px-5 cursor-pointer
                hover:bg-indigo-9 transition-250">Enviar</span
                >
            </div>
        </div>
        {#each data.posts.sort((a, b) => new Date(b.date).getTime() - new Date(a.date).getTime()) as post (post.id)}
            <Post data={post} />
        {/each}
    </div>
</div>
