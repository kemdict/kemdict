<script>
  export let initialInput = undefined;
  export let wantHeader = true;
  export let leftFirst = true;
  import Header from "$lib/Header.svelte";
</script>

{#if wantHeader}
  <Header search={true} {initialInput} />
{/if}

{#if leftFirst}
  <div class="split-layout">
    <div>
      <div class="left max-sm:mb-6">
        <slot name="left" />
      </div>
      <slot name="right" />
    </div>
  </div>
{:else}
  <div class="split-layout">
    <div>
      <slot name="right" />
      <div class="left max-sm:mt-8">
        <slot name="left" />
      </div>
    </div>
  </div>
{/if}
