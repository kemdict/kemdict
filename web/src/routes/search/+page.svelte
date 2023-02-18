<script>
  export let data;

  import { browser } from "$app/environment";
  import { Tabs, Tab, TabList, TabPanel } from "svelte-tabs";

  import Elsewhere from "$lib/Elsewhere.svelte";
  import SingleLayout from "$lib/SingleLayout.svelte";
  import Spinner from "$lib/Spinner.svelte";
  import WordPreview from "$lib/WordPreview.svelte";
  import { dictsByLang } from "$lib/common.js";

  $: heteronyms = data.heteronyms;

  function getTitle(mtch, query) {
    if (mtch === "prefix") {
      return `以「${query}」開頭的詞`;
    } else if (mtch === "suffix") {
      return `以「${query}」結尾的詞`;
    } else if (mtch === "contains") {
      return `包含「${query}」的詞`;
    }
  }

  $: title = getTitle(data.match, data.query);
</script>

<svelte:head>
  <title>{title} - kemdict 搜尋結果</title>
  <meta name="description" content="搜尋 kemdict" />
</svelte:head>

<SingleLayout initialMatchSelection={data.match} initialInput={data.query}>
  {#if data.count === 0}
    <div class="prose">
      <h1>「{data.query}」的搜尋結果</h1>
      <p>找不到{title}。</p>
      <hr />
      <Elsewhere term={data.query} />
    </div>
  {:else if data.count !== 0}
    <h1 class="mt-8 text-2xl font-bold">{title}</h1>
    <p class="text-sm">
      共 {data.count} 個定義
    </p>
    {#if browser}
      <div class="mt-4">
        <Tabs>
          <TabList>
            {#each data.langs as [langId, lang]}
              <Tab>{lang}</Tab>
            {/each}
          </TabList>
          {#each data.langs as [langId, lang]}
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
    {:else}
      <div class="text-center">
        <Spinner />
      </div>
    {/if}
  {/if}
</SingleLayout>
