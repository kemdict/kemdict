<script>
  export let search = false;
  export let initialInput = undefined;
  export let initialMatchSelection = undefined;
  import DarkModeToggle from "$lib/DarkModeToggle.svelte";
  import SearchBar from "$lib/SearchBar.svelte";
</script>

<div class="mb-8">
  <nav class="my-2 flex items-center justify-between">
    <a class="font-bold hover:underline" href="/">Kemdict 國語整合典 (β)</a>
    <div class="text-right">
      <DarkModeToggle />
    </div>
  </nav>
  {#if search}
    <SearchBar {initialInput} {initialMatchSelection} />
  {/if}
</div>
