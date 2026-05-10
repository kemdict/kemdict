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
    <div class="h-5 w-5"></div>
  {:else if starred}<svg
      class="text-primary h-5 w-5"
      xmlns="http://www.w3.org/2000/svg"
      width="512"
      height="512"
      viewBox="0 0 512 512"
      ><path
        d="M394,480a16,16,0,0,1-9.39-3L256,383.76,127.39,477a16,16,0,0,1-24.55-18.08L153,310.35,23,221.2A16,16,0,0,1,32,192H192.38l48.4-148.95a16,16,0,0,1,30.44,0l48.4,149H480a16,16,0,0,1,9.05,29.2L359,310.35l50.13,148.53A16,16,0,0,1,394,480Z"
      /></svg
    >{:else}<svg
      class="text-primary h-5 w-5"
      xmlns="http://www.w3.org/2000/svg"
      width="512"
      height="512"
      viewBox="0 0 512 512"
      ><path
        d="M480,208H308L256,48,204,208H32l140,96L118,464,256,364,394,464,340,304Z"
        style="fill:none;stroke-linejoin:round;stroke-width:32px"
      /></svg
    >{/if}</button
>
