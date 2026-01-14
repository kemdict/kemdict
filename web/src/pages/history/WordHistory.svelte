<script lang="ts">
  import WordList from "$src/components/WordList.svelte";
  import { lGet } from "$src/localStorage";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";

  const wordHistory: Writable<string[]> = localStorageStore("wordHistory", []);
  const searchHistory: Writable<string[]> = localStorageStore(
    "searchHistory",
    [],
  );
  function guardGet(key: string) {
    const value = lGet(key);
    if (Array.isArray(value)) {
      return value;
    } else {
      return [];
    }
  }
  let copyWords: HTMLButtonElement;
  let copySearch: HTMLButtonElement;
</script>

<div class="sm:mt-4 sm:grid sm:grid-cols-2">
  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-4">詞彙紀錄</h2>
    <div class="mb-2 space-x-1">
      <button
        disabled={$wordHistory.length === 0}
        onclick={() => {
          $wordHistory = [];
        }}
        class="variant-filled btn">刪除詞彙紀錄</button
      >
      <button
        bind:this={copyWords}
        disabled={$wordHistory.length === 0}
        onclick={() => {
          navigator.clipboard.writeText(
            JSON.stringify($wordHistory.map((str) => decodeURI(str))),
          );
          const oldInnerText = copyWords.innerText;
          copyWords.innerText = "已複製！";
          setTimeout(() => {
            copyWords.innerText = oldInnerText;
          }, 1000);
        }}
        class="variant-filled btn">匯出詞彙紀錄</button
      >
    </div>
    {#if $wordHistory.length > 0}
      <WordList
        words={[...$wordHistory].map(decodeURIComponent).reverse()}
        search={false}
      ></WordList>
    {/if}
  </div>

  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-8">搜尋紀錄</h2>
    <div class="mb-2 space-x-1">
      <button
        disabled={$searchHistory.length === 0}
        onclick={() => {
          $searchHistory = [];
        }}
        class="variant-filled btn">刪除搜尋紀錄</button
      >
      <button
        bind:this={copySearch}
        disabled={$searchHistory.length === 0}
        onclick={() => {
          navigator.clipboard.writeText(
            JSON.stringify($searchHistory.map((str) => decodeURI(str))),
          );
          const oldInnerText = copySearch.innerText;
          copySearch.innerText = "已複製！";
          setTimeout(() => {
            copySearch.innerText = oldInnerText;
          }, 1000);
        }}
        class="variant-filled btn">匯出搜尋紀錄</button
      >
    </div>
    {#if $searchHistory.length > 0}
      <WordList
        words={[...$searchHistory].reverse()}
        search={false}
        prefix="/search/?q="
      ></WordList>
    {/if}
  </div>
</div>
