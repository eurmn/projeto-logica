<script lang="ts">
    import Dash from '$lib/components/Dash.svelte';
</script>

<div class="grid grid-cols-[auto_1fr] h-full w-full">
    <Dash />
    <slot />
</div>
