<script>
  export let sort
</script>

<form action="/search" method="GET">
  <label>
    <input
      type="radio"
      bind:group={sort}
      value="asc"
      id="ascend"
      name="s"
      checked={sort !== "desc"}
    />
    遞增</label
  >
  <label>
    <input
      type="radio"
      bind:group={sort}
      value="desc"
      id="descend"
      name="s"
      checked={sort === "desc"}
    />
    遞減</label
  >
  <!-- With this, this even works with EWW. -->
  <noscript>
    <input type="hidden" name="q" value={query} />
    <input type="submit" value="重新搜尋" />
  </noscript>
</form>
