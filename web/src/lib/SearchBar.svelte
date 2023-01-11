<script>
  import { browser } from "$app/environment";
  export let initialInput = "";
  export let initialMatchSelection = "prefix";
  let input;
</script>

<!-- Following the same approach as kit.svelte.dev/src/lib/search/SearchBox.svelte, with some hints from MDN's page about navigator.platform -->

<svelte:window
  on:keydown={(event) => {
    let modifier = "ctrlKey";
    if (
      navigator.platform.indexOf("Mac") === 0 ||
      navigator.platform === "iPhone"
    ) {
      modifier = "metaKey";
    }
    if (event.key === "k" && event[modifier]) {
      if (input) {
        input.focus();
        event.preventDefault();
      }
    }
  }}
/>

<div class="relative mt-2 mb-2">
  {#if browser}
    <form action="/search" class="flex h-8" id="sf">
      <!-- TODO: add keybind hint -->
      <select class="btnColor rounded-l" name="m">
        <option selected={initialMatchSelection == "prefix"} value="prefix"
          >開頭為</option
        >
        <option selected={initialMatchSelection == "suffix"} value="suffix"
          >結尾為</option
        >
        <option selected={initialMatchSelection == "contains"} value="contains"
          >包含</option
        >
      </select>
      <input
        class="fade peer w-full border-b-2 border-gray-200 bg-white hover:border-gray-300 dark:border-stone-800 dark:bg-stone-900 dark:hover:border-stone-700"
        type="search"
        autocomplete="off"
        placeholder="輸入欲搜尋的詞彙"
        name="q"
        bind:this={input}
        value={initialInput}
      />
      <!-- /search?q=<...>, like search engines -->
      <input class="btnColor rounded-r" type="submit" value="搜尋" />
      <div
        class="absolute right-14 top-[calc(50%-0.9rem)] text-right peer-hover:hidden peer-focus:hidden max-sm:hidden"
      >
        <kbd>Ctrl</kbd> <kbd>K</kbd>
      </div>
    </form>
  {:else}
    <div class="h-8">
      <svg
        class="inline h-5 w-5 animate-spin text-gray-700"
        xmlns="http://www.w3.org/2000/svg"
        fill="none"
        viewBox="0 0 24 24"
      >
        <circle
          class="opacity-25"
          cx="12"
          cy="12"
          r="10"
          stroke="currentColor"
          stroke-width="4"
        />
        <path
          class="opacity-75"
          fill="currentColor"
          d="M4 12a8 8 0 018-8V0C5.373 0 0 5.373 0 12h4zm2 5.291A7.962 7.962 0 014 12H0c0 3.042 1.135 5.824 3 7.938l3-2.647z"
        />
      </svg>
    </div>
  {/if}
</div>
