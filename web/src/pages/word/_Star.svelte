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

  function addHandler() {
    $favorites.push(word)
  }
  function removeHandler() {
    // word and words in the array are both encoded
    $favorites = $favorites.filter((val) => val !== word)
  }
  let starred = $derived($favorites.includes(word))
</script>

<button
  class="text-lg"
  disabled={!globalThis.location}
  onclick={starred ? removeHandler : addHandler}>{#if starred}★{:else}☆{/if}}</button
>
