<script>
  export let langs = ["id", "readablename"];
  export let heteronyms = [];
  import { dictsByLang } from "$src/common";
  import { Tabs, Tab, TabList, TabPanel } from "svelte-tabs";
  import WordPreview from "$src/components/WordPreview.svelte";
  import Spinner from "$src/components/Spinner.svelte";
  import { onMount } from "svelte";
  // Thanks https://stackoverflow.com/a/75274563
  onMount(() => {
    const spinner = document.getElementById("spinner");
    spinner.parentNode.removeChild(spinner);
    document.getElementById("tabs").classList.toggle("hidden");
  });
</script>

<div class="mt-4">
  <div class="text-center" id="spinner">
    <Spinner />
  </div>
  <div id="tabs" class="hidden">
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
</div>
