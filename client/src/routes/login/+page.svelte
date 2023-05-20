<script lang="ts">
    import { goto } from '$app/navigation';
    import { onMount } from 'svelte';
    import { Avatars } from '$lib/utils';

    let username: string;
    let password: string;
    let avatarId = 0;

    let isSignup = false;
    let params: URLSearchParams;
    
    onMount(() => {
      params = new URLSearchParams(window.location.search);
    });

    async function login() {
      if (!username || !password) {
        return;
      }
      
      let res = await fetch('/api/login', {
        method: 'POST',
        credentials: 'include',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ username, password }),
      });

      if (res.ok) {
        goto(params.get('redirect') || '/');
      }
    }

    async function signup() {
      if (!username || !password) {
        return;
      }
      
      let res = await fetch('/api/signup', {
        method: 'POST',
        credentials: 'include',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ username, password, avatarId }),
      });

      if (res.ok) {
        goto(params.get('redirect') || '/');
      }
    }
</script>

<div
    class="w-full h-full grid place-items-center bg-gradient-to-b from-indigo-9 to-indigo-950"
>
    <div
        class="bg-zinc-9 max-w-lg max-h-2xl w-full h-full rounded-5
        flex flex-col gap-4 text-zinc-2 p-10 shadow-lg shadow-black/20"
    >
        <div class="text-center mb-7">
            <h1 class="mb-1">{isSignup ? 'Crie sua conta': 'Inicie sua sessão'}</h1>
            <div class="text-sm text-zinc-5">Preencha seus dados abaixo</div>
        </div>
        <div class="flex flex-col gap-6">
            <div class="flex flex-col gap-2 text-zinc-3">
                <div class="font-semibold">Nome de Usuário</div>
                <input
                    bind:value={username}
                    placeholder="Usuario"
                    type="text"
                    class="font-semibold bg-zinc-8 border-none outline-none
                text-white px-5 py-4 rounded-lg placeholder:text-zinc-5"
                />
            </div>
            <div class="flex flex-col gap-2 text-zinc-3">
                <div class="font-semibold">Senha</div>
                <input
                    bind:value={password}
                    placeholder="Senha"
                    type="password"
                    class="font-semibold bg-zinc-8 border-none outline-none
                text-white px-5 py-4 rounded-lg placeholder:text-zinc-5"
                />
            </div>
            {#if isSignup}
                <div class="flex flex-col gap-4">
                    <div class="font-semibold">Escolha seu avatar</div>
                    <div class="grid justify-evenly grid-flow-col text-white overflow-hidden
                    [&>*]:cursor-pointer gap-3">
                    <!-- svelte-ignore a11y-click-events-have-key-events -->
                    {#each Avatars as avatar, i}
                            <img
                                on:click={() => avatarId = i}
                                width="100"
                                height="100"
                                src={avatar.src}
                                alt="{avatar.alt} Avatar"
                                class="{avatarId === i ? 'border-indigo-6' : 'border-zinc-5'}
                                    w-20 h-20 rounded-full object-cover border-2 border-solid transition-250"
                            />
                        {/each}
                    </div>
                </div>
            {/if}
            <div class="flex flex-col gap-3">
                <!-- svelte-ignore a11y-click-events-have-key-events -->
                <div
                    on:click={() => isSignup ? signup() : login()}
                    class="px-6 py-3 rounded-lg bg-indigo-6 text-center cursor-pointer
                hover:brightness-80 transition-250"
                >
                    {isSignup ? 'criar conta' : 'iniciar sessão'}
                </div>
                <div class="w-full grid grid-cols-2 gap-2">
                    <!-- svelte-ignore a11y-click-events-have-key-events -->
                    <div
                        on:click={() => isSignup = !isSignup}
                        class="px-6 py-3 rounded-lg border-solid border-2 border-indigo-8 items-center
                    text-center cursor-pointer hover:brightness-80 transition-250 flex gap-2 justify-center"
                    >
                        <div class="i-heroicons-user-solid" />
                        <span>{isSignup ? 'já tenho conta' : 'criar uma conta'}</span>
                    </div>
                    <div
                        class="px-6 py-3 rounded-lg border-solid border-2 border-indigo-8 items-center
                    text-center cursor-pointer hover:brightness-80 transition-250 flex gap-2 justify-center"
                    >
                        <div class="i-heroicons-lock-closed-20-solid" />
                        <span>esqueci a senha</span>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>
