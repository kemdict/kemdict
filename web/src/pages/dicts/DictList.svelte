<script lang="ts">
  export let currentId: string;

  import { dicts, langIdName } from "common";
  import clsx from "clsx";
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
  {#each dicts.filter((d) => !d.hidden) as { id, name, meta, lang }}
    {#if filterString === "" || needle(name, meta.desc, langIdName(lang))
        .toLowerCase()
        .includes(filterString.toLowerCase())}
      <li>
        <a
          href="/dicts/{id}"
          class={clsx(
            "k-card mr-2 block px-2 py-1",
            id === currentId && "bg-surface-active-token ",
          )}
        >
          <div>
            <span class="font-bold">{name}</span>
            {#if meta.version}
              <span class="text-secondary-800-100-token text-sm"
                >{meta.version}</span
              >
            {/if}
          </div>
        </a>
      </li>
    {/if}
  {/each}
</ul>
