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
  {#each presentLangs.slice(0, 7) as langId}
    <Tab bind:group={currentTab} name={langId} value={langId}>
      <div class="block px-4 py-2">{langs[langId as LangId]}</div>
    </Tab>
  {/each}
  {#if presentLangs.length > 7}
    <div class="ml-2 flex items-center">
      <button
        class="btn btn-sm variant-filled"
        type="button"
        title="其他語言"
        use:popup={{
          event: "click",
          target: "popupLangs",
          placement: "bottom",
          closeQuery: ".listbox-item",
        }}
        >{presentLangs.indexOf(requestedLang) >= 7
          ? langs[requestedLang as LangId]
          : ""}…</button
      >
      <div class="card z-10 p-4 shadow-xl" data-popup="popupLangs">
        <ListBox padding="">
          {#each presentLangs.slice(7) as langId}
            <ListBoxItem group={requestedLang} name="lang" value={langId}>
              <a
                class="block px-4 py-2"
                href="/word/{title}{paramsToString(langId)}">{langs[langId]}</a
              >
            </ListBoxItem>
          {/each}
        </ListBox>
        <div class="arrow bg-surface-100-800-token"></div>
      </div>
    </div>
  {/if}
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
