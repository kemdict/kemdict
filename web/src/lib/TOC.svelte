<script>
  export let presentDicts = [];
  export let term = undefined;
  import { dicts } from "$lib/common";
</script>

{#if presentDicts.length > 1}
  <div class="prose mb-8">
    <h2 class="mb-2 mt-0">
      {#if term}
        有收錄「{term}」的字典
      {:else}
        字典
      {/if}
    </h2>
    <ul>
      {#each dicts as dict}
        {#if presentDicts.indexOf(dict.id) !== -1}
          <li><a href="#{dict.id}">{dict.name}</a></li>
        {/if}
      {/each}
    </ul>
  </div>
{/if}
