<script lang="ts">
  import WordList from "$src/components/WordList.svelte";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";
  const searchHistory: Writable<string[]> = localStorageStore(
    "searchHistory",
    [],
  );
  import CopyButton from "$src/components/CopyButton.svelte";
</script>

<div class="sm:mt-4 sm:grid sm:grid-cols-2">
  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-8">搜尋紀錄</h2>
    <p class="mb-2">紀錄只保留在瀏覽器裡，沒有傳到伺服器上。</p>
    <div class="mb-2 space-x-1">
      <button
        disabled={$searchHistory.length === 0}
        onclick={() => {
          $searchHistory = [];
        }}
        class="variant-filled btn">刪除搜尋紀錄</button
      >
      <CopyButton
        disabled={$searchHistory.length === 0}
        label="匯出搜尋紀錄"
        getstr={() =>
          JSON.stringify($searchHistory.map((str) => decodeURI(str)))}
      />
    </div>
    {#if $searchHistory.length > 0}
      <WordList
        words={[...$searchHistory].reverse()}
        search={false}
        prefix="/search/?q="
        deleter={(word) => {
          $searchHistory = $searchHistory.filter((val) => val !== word);
        }}
      ></WordList>
    {/if}
  </div>
</div>
