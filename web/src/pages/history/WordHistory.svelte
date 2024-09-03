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
</script>

<div class="sm:mt-4 sm:grid sm:grid-cols-2">
  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-4">詞彙紀錄</h2>
    <button
      disabled={$wordHistory.length === 0}
      on:click={() => {
        $wordHistory = [];
      }}
      class="btn variant-filled">刪除詞彙紀錄</button
    >
    {#if $wordHistory.length > 0}
      <WordList
        words={[...$wordHistory].map(decodeURIComponent).reverse()}
        search={false}
      ></WordList>
    {/if}
  </div>

  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-8">搜尋紀錄</h2>
    <button
      disabled={$searchHistory.length === 0}
      on:click={() => {
        $searchHistory = [];
      }}
      class="btn variant-filled">刪除搜尋紀錄</button
    >
    {#if $searchHistory.length > 0}
      <WordList
        words={[...$searchHistory].reverse()}
        search={false}
        prefix="/search/?q="
      ></WordList>
    {/if}
  </div>
</div>
