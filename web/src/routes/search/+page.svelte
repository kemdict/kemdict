<script>
  export let data;

  import Elsewhere from "$lib/Elsewhere.svelte";
  import SingleLayout from "$lib/SingleLayout.svelte";
  import SortForm from "./SortForm.svelte";
  import WordPreview from "$lib/WordPreview.svelte";
  import { WordSortFns } from "$lib/common.js";

  function getTitle(mtch, query) {
    if (mtch === "prefix") {
      return `以「${query}」開頭的詞`;
    } else if (mtch === "suffix") {
      return `以「${query}」結尾的詞`;
    } else if (mtch === "contains") {
      return `包含「${query}」的詞`;
    }
  }
  function getTitleForPronunciation(mtch, query) {
    if (mtch === "prefix") {
      return `讀音以「${query}」開頭的詞`;
    } else if (mtch === "suffix") {
      return `讀音以「${query}」結尾的詞`;
    } else if (mtch === "contains") {
      return `讀音包含「${query}」的詞`;
    }
  }

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
  $: title = getTitle(data.match, data.query);
  $: titlePronunciation = getTitleForPronunciation(data.match, data.query);
</script>

<svelte:head>
  <title>{title} - kemdict 搜尋結果</title>
  <meta name="description" content="搜尋 kemdict" />
</svelte:head>

<SingleLayout initialMatchSelection={data.match} initialInput={data.query}>
  {#if data.count === 0 && data.countPn === 0}
    <div class="prose">
      <h1>「{data.query}」的搜尋結果</h1>
      <p>找不到{title}。</p>
      <hr />
      <Elsewhere term={data.query} />
    </div>
  {:else}
    {#if data.count !== 0}
      <h1 class="mt-8 text-2xl font-bold">{title}</h1>
      <h2 class="text-sm">
        共 {data.count} 個定義
      </h2>
      <SortForm bind:sort query={data.query} />
      <ul>
        {#each data.words.sort(sortFn) as word}
          <WordPreview {word} />
        {/each}
      </ul>
    {/if}
    {#if data.countPn !== 0}
      <h1 class="mt-8 text-2xl font-bold">{titlePronunciation}</h1>
      <h2 class="text-sm">
        共 {data.countPn} 個定義
      </h2>
      <SortForm bind:sort={sortPn} query={data.query} />
      <ul>
        {#each data.wordsPn.sort(sortPnFn) as word}
          <WordPreview {word} />
        {/each}
      </ul>
    {/if}
  {/if}
</SingleLayout>
