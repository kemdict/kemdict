<script>
  export let term;

  import Out from "./Out.svelte";
</script>

<div class="prose">
  <h2 class="mb-2 mt-0">在其他地方搜尋「{term}」</h2>
  <ul>
    <li>
      <Out href="https://google.com/search?q={term}">Google</Out>
    </li>
    <li>
      <Out href="https://zh.wikipedia.org/w/index.php?search={term}"
        >維基百科</Out
      >
    </li>
    <li>
      <Out href="https://zh.wiktionary.org/w/index.php?search={term}"
        >維基詞典</Out
      >
    </li>
    <li>
      <Out href="https://moedict.tw/{term}">萌典</Out>
    </li>
    <li>
      <Out
        href="https://terms.naer.edu.tw/search/?match_type=phrase&query_op=&query_field=title&query_term={term}"
        >學術名詞資料庫</Out
      >
    </li>
    <li>
      <Out href="https://www.weblio.jp/content_find?query={term}">Weblio</Out>
    </li>
  </ul>
</div>
