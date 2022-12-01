<script>
  export let initialInput = "";
</script>

<div id="sbc">
  <form action="/search" id="sf">
    <input
      id="sb"
      type="search"
      autocomplete="off"
      placeholder="輸入詞彙"
      name="q"
      value={initialInput}
    />
    <!-- /search?q=<...>, like search engines -->
    <input type="submit" value="搜尋" />
  </form>
</div>
