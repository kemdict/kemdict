<script>
  import Header from "$lib/components/Header.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  import WordPreview from "$lib/components/WordPreview.svelte";
  import Word from "$lib/components/Word.svelte";
  export let data;
  import { groupByProp, dicts } from "$lib/common.js";
  let words = data.words;
  for (let w of words) {
    w.dict = Object.keys(dicts).find((x) => w[x]);
  }
  let count = false;
  let increment = () => {
    if (!count) {
      count = 0;
    }
    count++;
  };
  let lst;
</script>

<svelte:head>
  <title>「{data.needle}」 - kemdict 搜尋結果</title>
  <meta name="description" content="搜尋 kemdict" />
</svelte:head>

<Header>
  <SearchBar initialInput={data.needle} />
</Header>

<h1 class="font-bold mt-8 text-2xl">以「{data.needle}」開頭的詞</h1>
<h2 class="text-sm">
  {#if lst}
    共 {lst.childElementCount} 個定義
  {:else}
    …
  {/if}
</h2>

<ul bind:this={lst}>
  {#each data.words as word}
    <WordPreview {word} />
  {/each}
</ul>
