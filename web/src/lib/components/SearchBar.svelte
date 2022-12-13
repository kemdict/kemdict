<script>
  export let initialInput = "";
  export let initialMatchSelection = "prefix";
</script>

<div class="relative mt-2 mb-2">
  <form action="/search" class="flex space-x-2" id="sf">
    <select class="rounded pl-2" name="m">
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
      class="w-full fade"
      type="search"
      autocomplete="off"
      placeholder="輸入欲搜尋的詞彙"
      name="q"
      value={initialInput}
    />
    <!-- /search?q=<...>, like search engines -->
    <input type="submit" value="搜尋" />
  </form>
</div>
