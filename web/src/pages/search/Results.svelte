<script>
  export let langGroups = [["id", "readablename"]];
  export let heteronyms = [];
  import { dictsByLang } from "$src/common";
  import { Tab, TabGroup } from "@skeletonlabs/skeleton";
  import WordPreview from "$src/components/WordPreview.svelte";
  export let currentLang = langGroups[0][0];
  export let currentParams = new URLSearchParams();
  function paramsToLink(langId) {
    currentParams.set("lang", langId);
    return "?" + currentParams.toString();
  }
</script>

<div class="mt-4">
  <div id="tabs">
    <TabGroup>
      {#each langGroups as [langId, lang]}
        <!-- We pass in the current tab just to get it to highlight the current tab correctly. Because we're using a separate page for each tab. -->
        <Tab group={currentLang} name={langId} value={langId} padding="">
          <a class="unstyled block px-4 py-2" href={paramsToLink(langId)}
            >{lang}</a
          >
        </Tab>
      {/each}
      <svelte:fragment slot="panel">
        <ul>
          {#each heteronyms as het}
            {#if dictsByLang[currentLang].includes(het.from)}
              <li><WordPreview heteronyms={[het]} /></li>
            {/if}
          {/each}
        </ul>
      </svelte:fragment>
    </TabGroup>
  </div>
</div>
