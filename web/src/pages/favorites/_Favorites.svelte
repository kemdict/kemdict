<script lang="ts">
  import WordList from "$src/components/WordList.svelte";
  import { lGet } from "$src/localStorage";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";

  const favorites: Writable<string[]> = localStorageStore("favorites", []);
  function guardGet(key: string) {
    const value = lGet(key);
    if (Array.isArray(value)) {
      return value;
    } else {
      return [];
    }
  }
  let copyFavorites: HTMLButtonElement;
</script>

<div class="sm:mt-4 sm:grid sm:grid-cols-2">
  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-4">收藏項目</h2>
    <p class="mb-2">收藏資訊只保留在瀏覽器裡，沒有傳到伺服器上。</p>
    <div class="mb-2 space-x-1">
      <button
        bind:this={copyFavorites}
        disabled={$favorites.length === 0}
        onclick={() => {
          navigator.clipboard.writeText(
            JSON.stringify($favorites.map((str) => decodeURI(str))),
          );
          const oldInnerText = copyFavorites.innerText;
          copyFavorites.innerText = "已複製！";
          setTimeout(() => {
            copyFavorites.innerText = oldInnerText;
          }, 1000);
        }}
        class="variant-filled btn">匯出收藏詞彙</button
      >
    </div>
    {#if $favorites.length > 0}
      <WordList
        words={[...$favorites].map(decodeURIComponent).reverse()}
        search={false}
        deleter={(word) => {
          $favorites = $favorites.filter(
            (val) => decodeURIComponent(val) !== word,
          );
        }}
      ></WordList>
    {/if}
  </div>
</div>
