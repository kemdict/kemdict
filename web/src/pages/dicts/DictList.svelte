<script lang="ts">
  import { dicts } from "common";
  dicts.sort((a, b) => (a.name < b.name ? -1 : 1));

  let filterString = "";
  import LocalSearchInput from "$src/components/LocalSearchInput.svelte";

  /**
   * Join `args` together to form a search needle.
   */
  function needle(...args: string[]) {
    return args.join("");
  }
</script>

<div class="mr-2">
  <LocalSearchInput bind:filterString />
</div>
<ul class="mt-2 space-y-2 overflow-auto">
  {#each dicts as { id, name, meta }}
    {#if filterString === "" || needle(name, meta.desc)
        .toLowerCase()
        .includes(filterString.toLowerCase())}
      <li>
        <a href="/dicts/{id}" class="card mr-2 block px-2 py-1">
          <div class="font-bold">{name}</div>
        </a>
      </li>
    {/if}
  {/each}
</ul>
