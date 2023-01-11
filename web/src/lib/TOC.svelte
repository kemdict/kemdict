<script>
  // Array of dict objects.
  export let presentDicts = [];
  export let term = undefined;
  import { langs, groupByProp } from "$lib/common";
</script>

{#if presentDicts.length > 1}
  <div class="mb-8">
    {#if term}
      <h1 class="mb-2 mt-0 text-2xl font-bold">
        {term}
      </h1>
    {/if}
    <div class="border p-2">
      {#each groupByProp(presentDicts, "lang") as [lang, dicts]}
        <h2 class="mt-2 font-bold first:mt-0">{langs[lang]}</h2>
        <ul>
          {#each dicts as dict}
            <li><a class="link ml-2" href="#{dict.id}">{dict.name}</a></li>
          {/each}
        </ul>
      {/each}
    </div>
  </div>
{/if}
