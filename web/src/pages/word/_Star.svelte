<script lang="ts">
  import WordList from "$src/components/WordList.svelte";
  import { lGet } from "$src/localStorage";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";

  // both are encoded
  const favorites: Writable<string[]> = localStorageStore("favorites", []);
  const loading = !globalThis.location;
  const word =
    globalThis.location?.href &&
    new URL(globalThis.location?.href).pathname.replace("/word/", "");

  let starred = $derived($favorites.includes(word));
  function toggle() {
    if (starred) {
      // word and words in the array are both encoded
      $favorites = $favorites.filter((val) => val !== word);
    } else {
      $favorites = [word, ...$favorites];
    }
  }
</script>

<!-- ionicons star (filled) and star-outline -->
<button
  class="btnColor btnBorder btn-icon p-1 text-lg"
  disabled={loading}
  onclick={toggle}
  type="button"
  title={loading ? "收藏" : starred ? "將詞彙從收藏中移除" : "將詞彙加入收藏"}
  aria-label={loading
    ? "收藏"
    : starred
      ? "將詞彙從收藏中移除"
      : "將詞彙加入收藏"}
  aria-checked={starred}
  >{#if loading}
    {"　"}
  {:else if starred}★{:else}☆{/if}</button
>
