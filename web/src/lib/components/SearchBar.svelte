<script>
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
  <form action="/search" class="flex" id="sf">
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
      class="w-full fade bg-white dark:bg-stone-900 border-b-2 border-gray-200 hover:border-gray-300 dark:border-stone-800 dark:hover:border-stone-700"
      type="search"
      autocomplete="off"
      placeholder="輸入欲搜尋的詞彙"
      name="q"
      bind:this={input}
      value={initialInput}
    />
    <!-- /search?q=<...>, like search engines -->
    <input class="btnColor rounded-r" type="submit" value="搜尋" />
  </form>
</div>
