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

<div>
  <div>語言</div>
  {#each presentLangs as langId}
    <div>
      <div class="block px-4 py-2">{langs[langId as LangId]}</div>
    </div>
  {/each}
</div>
<div class="relative">
  <div class="text-right">
    <Star />
  </div>
  {#each presentLangs as thisLang}
    <div class="mb-4 group">
      <h1 id={thisLang} class="font-serif text-2xl">
        <button
          class="w-full text-left"
          on:click={(e) => {
            (
              e.target as HTMLButtonElement
            ).parentElement?.parentElement?.classList.toggle("collapsed");
          }}
        >
          {langs[thisLang as LangId]}
        </button>
      </h1>
      <hr class="w-full my-2 border-surface-600 dark:border-surface-900" />
      <div class="group-[.collapsed]:hidden">
        {#each groupedHets as [dict, hets]}
          {#if thisLang === dict.lang}
            <Word groupedHets={[[dict, hets]]} {title} />
          {/if}
        {/each}
      </div>
    </div>
  {/each}
</div>
