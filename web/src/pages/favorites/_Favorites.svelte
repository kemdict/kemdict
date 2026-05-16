<script lang="ts">
  import VerticalWordList from "$src/components/VerticalWordList.svelte";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";
  const favorites: Writable<string[]> = localStorageStore("favorites", []);
  import CopyButton from "$src/components/CopyButton.svelte";
</script>

<div class="sm:mt-4 sm:grid sm:grid-cols-2">
  <div>
    <h2 class="mb-2 text-xl font-bold max-sm:mt-4">收藏項目</h2>
    <p class="mb-2">收藏資訊只保留在瀏覽器裡，沒有傳到伺服器上。</p>
    <div class="mb-2 space-x-1">
      <CopyButton
        disabled={$favorites.length === 0}
        label="匯出收藏詞彙"
        getstr={() => JSON.stringify($favorites.map((str) => decodeURI(str)))}
      />
    </div>
    {#if $favorites.length > 0}
      <VerticalWordList
        words={[...$favorites].map(decodeURIComponent).reverse()}
        search={false}
        deleter={(word) => {
          $favorites = $favorites.filter(
            (val) => decodeURIComponent(val) !== word,
          );
        }}
      ></VerticalWordList>
    {/if}
  </div>
</div>
