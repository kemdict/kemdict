<script lang="ts">
  export let presentLangs: string[] = [];
  export let requestedLang: string;
  export let groupedHets: [Dict, Heteronym[]][];
  export let title: string;
  export let currentParams: URLSearchParams;

  function paramsToString(params: URLSearchParams, langId: string) {
    params.set("lang", langId);
    return "?" + params.toString();
  }

  import { Tab, TabGroup } from "@skeletonlabs/skeleton";
  import { langs } from "common";
  import type { Dict, Heteronym } from "common";
  import Word from "./Word.svelte";
</script>

<TabGroup>
  {#each presentLangs as langId}
    <Tab group={requestedLang} name={langId} value={langId}>
      <a
        class="unstyled block px-4 py-2"
        href="/word/{title}{paramsToString(currentParams, langId)}"
        >{langs[langId]}</a
      >
    </Tab>
  {/each}
  <svelte:fragment slot="panel">
    {#each groupedHets as [dict, hets]}
      {#if requestedLang === dict.lang}
        <Word groupedHets={[[dict, hets]]} {title} />
      {/if}
    {/each}
  </svelte:fragment>
</TabGroup>
