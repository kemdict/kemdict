<script>
  import Header from "$lib/components/Header.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  import WordPreview from "$lib/components/WordPreview.svelte";
  import Elsewhere from "$lib/components/Elsewhere.svelte";
  import SortBtn from "SortBtn.svelte";
  export let data;
  import { WordSortFns } from "$lib/common.js";
  let sortFn;
  let sortPnFn;
  let sort = data.sort;
  let sortPn = data.sort;
  $: if (sort === "desc") {
    sortFn = WordSortFns.descend;
  } else {
    sortFn = WordSortFns.ascend;
  }
  $: if (sortPn === "desc") {
    sortPnFn = WordSortFns.descend;
  } else {
    sortPnFn = WordSortFns.ascend;
  }
  function matchType(mtch, word) {
    if (mtch === "prefix") {
      return `以「${word}」開頭的詞`;
    } else if (mtch === "suffix") {
      return `以「${word}」結尾的詞`;
    } else if (mtch === "contains") {
      return `包含「${word}」的詞`;
    }
  }
</script>

<svelte:head>
  <title>{matchType(data.match, data.query)} - kemdict 搜尋結果</title>
  <meta name="description" content="搜尋 kemdict" />
</svelte:head>

<Header>
  <SearchBar initialMatchSelection={data.match} initialInput={data.query} />
</Header>

{#if data.count === 0 && data.countPn === 0}
  <p class="mt-8">找不到{matchType(data.match, data.query)}</p>
  <Elsewhere term={data.query} />
{:else}
  {#if data.count !== 0}
  <h1 class="font-bold mt-8 text-2xl">{matchType(data.match, data.query)}</h1>
  <h2 class="text-sm">
    共 {data.count} 個定義
  </h2>
  <SortForm bind:sort={sort} {query} />
  <ul>
    {#each data.words.sort(sortFn) as word}
      <WordPreview {word} />
    {/each}
  </ul>
  {/if}
  {#if data.countPn !== 0}
  <h1 class="font-bold mt-8 text-2xl">讀音包含「{data.query}」的詞</h1>
  <h2 class="text-sm">
    共 {data.countPn} 個定義
  </h2>
  <SortForm bind:sort={sortPn} {query} />
  <ul>
    {#each data.wordsPn.sort(sortPnFn) as word}
      <WordPreview {word} />
    {/each}
  </ul>
  {/if}
{/if}
