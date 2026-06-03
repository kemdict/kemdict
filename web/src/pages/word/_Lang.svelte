<script lang="ts">
  import { langs } from "common";
  import type { Dict, LangId, Heteronym } from "common";
  import Word from "./_Word.svelte";
  interface Props {
    groupedHets: [Dict, Heteronym[]][];
    title: string;
    langId: LangId;
  }
  const { groupedHets, title, langId }: Props = $props();

  let collapsed = $state(false);
</script>

<div class={["mb-4 group", collapsed && "collapsed"]}>
  <h1 id="lang-{langId}" class="font-serif text-2xl scroll-m-[75px]">
    <button
      class="w-full text-left"
      onclick={() => {
        collapsed = !collapsed;
      }}
    >
      <!-- either right = collapsed, down = expanded (current state) -->
      <!-- or down = collapsed, up = expanded (what it'll do) -->
      <!-- For up/down, up = collapsed is quite weird and rare. -->
      <span class="opacity-50"
        >{#if collapsed}⮞{:else}⮟{/if}</span
      >
      {langs[langId]}
    </button>
  </h1>
  <!-- We need the divider class so the colors here would actually apply -->
  <hr class="w-full my-2 divider border-surface-600 dark:border-surface-900" />
  <div class="group-[.collapsed]:hidden">
    {#each groupedHets as [dict, hets]}
      {#if langId === dict.lang}
        <Word groupedHets={[[dict, hets]]} {title} />
      {/if}
    {/each}
  </div>
</div>
