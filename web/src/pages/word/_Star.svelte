<script lang="ts">
  import WordList from "$src/components/WordList.svelte";
  import { lGet } from "$src/localStorage";
  import { localStorageStore } from "@skeletonlabs/skeleton";
  import type { Writable } from "svelte/store";

  // both are encoded
  const favorites: Writable<string[]> = localStorageStore("favorites", []);
  const word =
    globalThis.location?.href &&
    new URL(globalThis.location?.href).pathname.replace("/word/", "");

  let starred = $derived($favorites.includes(word))
  function toggle() {
    if (starred) {
      // word and words in the array are both encoded
      $favorites = $favorites.filter((val) => val !== word)
    } else {
      $favorites = [word, ...$favorites]
    }
  }
</script>

<button
  class="text-lg btn-icon variant-soft-surface"
  disabled={!globalThis.location}
  onclick={toggle}>{#if starred}★{:else}☆{/if}</button
>
