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
      class="w-full text-left flex gap-x-1"
      onclick={() => {
        collapsed = !collapsed;
      }}
    >
      <!-- either right = collapsed, down = expanded (current state) -->
      <!-- or down = collapsed, up = expanded (what it'll do) -->
      <!-- For up/down, up = collapsed is quite weird and rare. -->
      <span class="opacity-75 inline-flex items-center"
        >{#if collapsed}
          <svg
            xmlns="http://www.w3.org/2000/svg"
            id="chevron-down-outline"
            class="ionicon inline size-4"
            viewBox="0 0 512 512"
            ><path
              fill="none"
              stroke="currentColor"
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="48"
              d="M112 184l144 144 144-144"
            /></svg
          >
        {:else}
          <svg
            xmlns="http://www.w3.org/2000/svg"
            class="ionicon inline size-4"
            viewBox="0 0 512 512"
            ><path
              fill="none"
              stroke="currentColor"
              stroke-linecap="round"
              stroke-linejoin="round"
              stroke-width="48"
              d="M112 328l144-144 144 144"
            /></svg
          >
        {/if}</span
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
