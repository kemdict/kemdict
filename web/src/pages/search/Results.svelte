<script>
  export let langGroups = [["id", "readablename"]];
  export let heteronyms = [];
  import { dictsByLang } from "$src/common";
  import { Tab, TabGroup } from "@skeletonlabs/skeleton";
  import WordPreview from "$src/components/WordPreview.svelte";
  let currentTab = langGroups[0][0];
</script>

<div class="mt-4">
  <div id="tabs">
    <TabGroup>
      {#each langGroups as [langId, lang]}
        <Tab bind:group={currentTab} name={langId} value={langId}>{lang}</Tab>
      {/each}
      <svelte:fragment slot="panel">
        {#if currentTab}
          <ul>
            {#each heteronyms as het}
              {#if dictsByLang[currentTab].includes(het.from)}
                <WordPreview heteronyms={[het]} />
              {/if}
            {/each}
          </ul>
        {/if}
      </svelte:fragment>
    </TabGroup>
  </div>
</div>
