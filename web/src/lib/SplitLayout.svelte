<script>
  export let initialInput = undefined;
  export let wantHeader = true;
  export let leftFirst = true;
  import SearchBar from "$lib/SearchBar.svelte";
  import Header from "$lib/Header.svelte";
</script>

{#if wantHeader}
  <Header>
    <SearchBar {initialInput} />
  </Header>
{/if}

<div class="split-layout">
  <div>
    {#if !leftFirst}
      <slot name="right" />
    {/if}
    <div class="left">
      <slot name="left" />
    </div>
    {#if leftFirst}
      <slot name="right" />
    {/if}
  </div>
</div>
