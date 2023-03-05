<script>
  export let langs = ["id", "readablename"];
  export let heteronyms = [];
  import { dictsByLang } from "$src/common";
  import { Tabs, Tab, TabList, TabPanel } from "svelte-tabs";
  import WordPreview from "$src/components/WordPreview.svelte";
  console.log(langs);
</script>

<div class="mt-4">
  <Tabs>
    <TabList>
      {#each langs as [langId, lang]}
        <Tab>{lang}</Tab>
      {/each}
    </TabList>
    {#each langs as [langId, lang]}
      <TabPanel>
        <ul>
          {#each heteronyms as het}
            {#if dictsByLang[langId].includes(het.from)}
              <WordPreview heteronyms={[het]} />
            {/if}
          {/each}
        </ul>
      </TabPanel>
    {/each}
  </Tabs>
</div>
