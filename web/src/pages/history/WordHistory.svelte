<script lang="ts">
  import { lGet } from "$src/localStorage";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";

  const wordHistory: Writable<string[]> = localStorageStore("wordHistory", []);
  const searchHistory: Writable<string[]> = localStorageStore(
    "searchHistory",
    []
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
      <ol class="mt-4">
        {#each $wordHistory as title}
          <li>
            <a class="wordlink" href="/word/{title}">{decodeURI(title)}</a>
          </li>
        {/each}
      </ol>
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
      <ol class="mt-4">
        {#each $searchHistory as query}
          <li>
            <a class="wordlink" href="/search/?q={query}">{query}</a>
          </li>
        {/each}
      </ol>
    {/if}
  </div>
</div>
