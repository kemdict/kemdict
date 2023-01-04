<script>
  export let availableDicts = [];
  import { dictsB } from "$lib/common";
</script>

{#if availableDicts.length > 1}
  <div class="prose mb-8">
    <h2 class="mb-2 mt-0">字典</h2>
    <ul>
      {#each dictsB as dict}
        {#if availableDicts.indexOf(dict.id) !== -1}
          <li><a href="#{dict.id}">{dict.name}</a></li>
        {/if}
      {/each}
    </ul>
  </div>
{/if}
