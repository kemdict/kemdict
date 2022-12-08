<script>
  import { browser } from "$app/environment";
  import Header from "$lib/components/Header.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  import WordPreview from "$lib/components/WordPreview.svelte";
  import Elsewhere from "$lib/components/Elsewhere.svelte";
  import Word from "$lib/components/Word.svelte";
  export let data;
  import { groupByProp, dicts, WordSort } from "$lib/common.js";
  let sortFn;
  let sort = data.sort;
  let urlParams;
  $: if (sort === "desc") {
    sortFn = WordSort.descend;
  } else {
    sortFn = WordSort.ascend;
  }
</script>

<svelte:head>
  <title>「{data.query}」 - kemdict 搜尋結果</title>
  <meta name="description" content="搜尋 kemdict" />
</svelte:head>

<Header>
  <SearchBar initialInput={data.query} />
</Header>

{#if data.count === 0}
  <p class="mt-8">找不到以「{data.query}」開頭的詞。</p>
  <Elsewhere term={data.query} />
{:else}
  <h1 class="font-bold mt-8 text-2xl">以「{data.query}」開頭的詞</h1>
  <h2 class="text-sm">
    共 {data.count} 個定義
  </h2>
  <form action="/search" method="GET">
    <label>
      <input
        type="radio"
        bind:group={sort}
        value="asc"
        id="ascend"
        name="s"
        checked={sort !== "desc"}
      />
      遞增</label
    >
    <label>
      <input
        type="radio"
        bind:group={sort}
        value="desc"
        id="descend"
        name="s"
        checked={sort === "desc"}
      />
      遞減</label
    >
    <!-- With this, this even works with EWW. -->
    <noscript>
      <input type="hidden" name="q" value={data.query} />
      <input type="submit" value="重新搜尋" />
    </noscript>
  </form>

  <ul>
    {#each data.words.sort(sortFn) as word}
      <WordPreview {word} />
    {/each}
  </ul>
{/if}
