<script lang="ts">
  import type { Heteronym } from "$src/common";
  import { Tab, TabGroup } from "@skeletonlabs/skeleton";
  import Pages from "$src/components/Pages.svelte";
  import WordPreview from "$src/components/WordPreview.svelte";
  export let pageCount: number;
  export let activePage: number;
  export let url: string | URL;
  export let langGroups = [["id", "readablename"]];
  // This is filtered before passed into this component
  export let heteronyms: Heteronym[] = [];
  export let langId: string;
  export let currentParamsString: string;
</script>

<div class="mt-4">
  <div id="tabs">
    <TabGroup>
      <Tab group={langId} name="all" value="all" padding="">
        <a
          class="unstyled block px-4 py-2"
          href="/search/?{currentParamsString}">所有辭典</a
        >
      </Tab>
      {#each langGroups as [id, name]}
        <!-- We pass in the current tab just to get it to highlight the current tab correctly. Because we're using a separate page for each tab. -->
        <Tab group={langId} name={id} value={id} padding="">
          <a
            class="unstyled block px-4 py-2"
            href="/search/{id}?{currentParamsString}">{name}</a
          >
        </Tab>
      {/each}
      <svelte:fragment slot="panel">
        <!-- TODO: client side page navigation -->
        <Pages {url} {activePage} {pageCount} />
        <ul>
          <WordPreview {heteronyms} />
        </ul>
      </svelte:fragment>
    </TabGroup>
  </div>
</div>
