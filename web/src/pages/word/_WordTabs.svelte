<script lang="ts">
  import {
    popup,
    Tab,
    TabGroup,
    ListBox,
    ListBoxItem,
  } from "@skeletonlabs/skeleton";
  import { langs } from "common";
  import type { Dict, LangId, Heteronym } from "common";
  import Word from "./_Word.svelte";
  import Star from "./_Star.svelte";

  export let presentLangs: string[] = [];
  export let requestedLang: string = "";
  export let groupedHets: [Dict, Heteronym[]][];
  export let title: string;
  export let currentParamString: string;
  const currentParams = new URLSearchParams(currentParamString);

  function paramsToString(langId: string) {
    if (currentParams) {
      currentParams.set("lang", langId);
    }
    return "?" + currentParams.toString();
  }

  let currentTab = requestedLang;
  const url =
    "document" in globalThis
      ? new URL(globalThis.document.location.href)
      : undefined;

  $: ((lang) => {
    (url?.searchParams.set("lang", lang),
      globalThis.window?.history.pushState(undefined, "", url));
  })(currentTab);
</script>

<TabGroup padding="">
  {#each presentLangs as langId}
    <Tab bind:group={currentTab} name={langId} value={langId}>
      <div class="block px-4 py-2">{langs[langId as LangId]}</div>
    </Tab>
  {/each}
  <svelte:fragment slot="panel">
    <div class="relative">
      <div class="absolute right-0 top-0">
        <Star />
      </div>
      {#each groupedHets as [dict, hets]}
        {#if currentTab === dict.lang}
          <Word groupedHets={[[dict, hets]]} {title} />
        {/if}
      {/each}
    </div>
  </svelte:fragment>
</TabGroup>
