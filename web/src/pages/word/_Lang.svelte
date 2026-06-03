<script lang="ts">
  import { langs } from "common";
  import type { Dict, LangId, Heteronym } from "common";
  import Word from "./_Word.svelte";
  export let groupedHets: [Dict, Heteronym[]][];
  export let title: string;
  export let langId: LangId;
</script>

<div class="mb-4 group">
  <h1 id="lang-{langId}" class="font-serif text-2xl scroll-m-[75px]">
    <button
      class="w-full text-left"
      on:click={(e) => {
        (
          e.target as HTMLButtonElement
        ).parentElement?.parentElement?.classList.toggle("collapsed");
      }}
    >
      {langs[langId]}
    </button>
  </h1>
  <hr class="w-full my-2 border-surface-600 dark:border-surface-900" />
  <div class="group-[.collapsed]:hidden">
    {#each groupedHets as [dict, hets]}
      {#if langId === dict.lang}
        <Word groupedHets={[[dict, hets]]} {title} />
      {/if}
    {/each}
  </div>
</div>
