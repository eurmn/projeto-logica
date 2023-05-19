<script lang="ts">
    let username: string;
    let password: string;

    async function login(username: string, password: string) {
      let res = await fetch('/api/login', {
        method: 'POST',
        headers: {
          'Content-Type': 'application/json',
        },
        body: JSON.stringify({ username, password }),
      });
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
            <h1 class="mb-1">Inicie sua sessão</h1>
            <div class="text-sm text-zinc-5">Por favor entre em sua conta</div>
        </div>
        <div class="flex flex-col gap-6">
            <div class="text-zinc-4">
                <span>Ainda não possui uma conta?</span>
                <span class="border-1 border-b-solid border-zinc-3 font-bold text-zinc-2">Crie agora.</span>
            </div>
            <div class="flex flex-col gap-2 text-zinc-3">
                <div class="font-semibold">Nome de Usuário</div>
                <input
                    bind:value={username}
                    placeholder="Usuario123"
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
            <div class="flex flex-col gap-3">
                <!-- svelte-ignore a11y-click-events-have-key-events -->
                <div
                    on:click={() => login(username, password)}
                    class="px-6 py-3 rounded-lg bg-indigo-6 text-center cursor-pointer
                hover:brightness-80 transition-250"
                >
                    iniciar sessão
                </div>
                <div class="w-full grid grid-cols-2 gap-2">
                    <div
                        class="px-6 py-3 rounded-lg border-solid border-2 border-indigo-8 items-center
                    text-center cursor-pointer hover:brightness-80 transition-250 flex gap-2"
                    >
                        <div class="i-heroicons-user-solid" />
                        <span>criar uma conta</span>
                    </div>
                    <div
                        class="px-6 py-3 rounded-lg border-solid border-2 border-indigo-8 items-center
                    text-center cursor-pointer hover:brightness-80 transition-250 flex gap-2"
                    >
                        <div class="i-heroicons-hashtag-20-solid" />
                        <span>esqueci a senha</span>
                    </div>
                </div>
            </div>
        </div>
    </div>
</div>
