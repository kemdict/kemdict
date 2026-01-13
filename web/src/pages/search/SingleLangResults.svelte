<script lang="ts">
  export let langCountObj: Record<LangId, number>;
  export let pageCount: number;
  export let activePage: number;
  export let url: string | URL;
  export let baseURL: string | URL = "";
  export let langGroups = [["id", "readablename"]];
  // This is filtered before passed into this component
  export let heteronymsAndPn: [Heteronym, string | undefined][] = [];
  export let langId: string;
  export let currentParamsString: string;
  export let searchQuery: string;

  import type { Heteronym, LangId } from "common";
  import { Tab, TabGroup } from "@skeletonlabs/skeleton";
  import Pages from "$src/components/Pages.svelte";
  import WordPreview from "$src/components/WordPreview.svelte";
</script>

<div class="mt-4">
  <div id="tabs">
    <TabGroup>
      <Tab group={langId} name="all" value="all" padding="">
        <a class="block px-4 py-2" href="/search/?{currentParamsString}"
          >所有辭典</a
        >
      </Tab>
      {#each langGroups as [id, name]}
        <!-- We pass in the current tab just to get it to highlight the current tab correctly. Because we're using a separate page for each tab. -->
        <Tab group={langId} name={id} value={id} padding="">
          <a class="block px-4 py-2" href="/search/{id}?{currentParamsString}"
            >{name}{#if langCountObj[id] > 1}
              ({langCountObj[id]}){/if}</a
          >
        </Tab>
      {/each}
      <svelte:fragment slot="panel">
        <!-- TODO: client side page navigation -->
        <Pages {baseURL} {url} {activePage} {pageCount} />
        <ul class="divide-y">
          <WordPreview {heteronymsAndPn} {searchQuery} />
        </ul>
        <Pages {baseURL} {url} {activePage} {pageCount} />
      </svelte:fragment>
    </TabGroup>
  </div>
</div>
