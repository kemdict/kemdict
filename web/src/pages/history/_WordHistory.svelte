<script lang="ts">
  import WordList from "$src/components/WordList.svelte";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";
  const wordHistory: Writable<string[]> = localStorageStore("wordHistory", []);
  let copyWords: HTMLButtonElement;
</script>

<div class="sm:mt-4 sm:grid sm:grid-cols-2">
  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-4">詞彙紀錄</h2>
    <p class="mb-2">紀錄只保留在瀏覽器裡，沒有傳到伺服器上。</p>
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
        deleter={(word) => {
          $wordHistory = $wordHistory.filter(
            (val) => decodeURIComponent(val) !== word,
          );
        }}
      ></WordList>
    {/if}
  </div>
</div>
