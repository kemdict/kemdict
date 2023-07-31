<script>
  import { onMount } from "svelte";
  /**
   * Normally we submit the search to `/search`. Use this to make it
   * `/search/foo`, for instance.
   */
  export let submitSuffix = "";
  export let initialInput = "";
  export let initialMatchSelection = "prefix";
  export let highlightBtn = false;
  export let redirectOnSingleResult = false;

  const matchTypes = new Map(
    Object.entries({
      prefix: "開頭為",
      suffix: "結尾為",
      contains: "包含",
      exact: "完全符合",
    })
  );
  onMount(() => {
    window.addEventListener("keydown", (e) => {
      // Following the same approach as
      // kit.svelte.dev/src/lib/search/SearchBox.svelte, with some hints
      // from MDN's page about navigator.platform
      let modifier = "ctrlKey";
      if (
        navigator.platform.indexOf("Mac") === 0 ||
        navigator.platform === "iPhone"
      ) {
        modifier = "metaKey";
      }
      if (e.key === "k" && e[modifier]) {
        document.getElementById("sbi").focus();
        e.preventDefault();
      }
    });
  });
</script>

<div class="relative mb-2 mt-2">
  <form action="/search{submitSuffix || ''}">
    {#if redirectOnSingleResult}
      <input type="hidden" name="r" />
    {/if}
    <div class="flex space-x-2">
      <div class="relative flex-grow">
        <input
          id="sbi"
          type="search"
          autocomplete="off"
          placeholder="輸入要搜尋的詞彙…"
          name="q"
          class="k-input w-full"
          value={initialInput}
        />
        <div class="absolute right-2 top-[25%] max-md:hidden">
          <kbd>Ctrl+K</kbd>
        </div>
      </div>
      <input
        class={highlightBtn ? "k-hlbtn" : "k-btn"}
        type="submit"
        value="搜尋"
      />
    </div>
    <div class="mt-4">
      <h2 class="mb-2 font-bold opacity-75">搜尋方式</h2>
      <fieldset class="flex max-w-[60%] flex-wrap gap-x-4 gap-y-2">
        {#each [...matchTypes] as [mtch, name]}
          <label class="flex items-center space-x-1">
            <input
              class="form-radio radio"
              type="radio"
              name="m"
              value={mtch}
              checked={mtch === initialMatchSelection}
            />
            <span>{name}</span>
          </label>
        {/each}
      </fieldset>
    </div>
  </form>
</div>
