<script>
  export let term;

  import Out from "./Out.svelte";
</script>

<div class="prose">
  <ul class="my-10">
    <li>
      <Out href="https://google.com/search?q={term}"
        >在 Google 搜尋「{term}」</Out
      >
    </li>
    <li>
      <Out href="https://zh.wiktionary.org/w/index.php?search={term}"
        >在維基詞典搜尋「{term}」</Out
      >
    </li>
    <li>
      <Out
        href="https://terms.naer.edu.tw/search/?match_type=phrase&query_op=&query_field=title&query_term={term}"
        >在學術名詞資料庫搜尋「{term}」</Out
      >
    </li>
    <li>
      <Out href="https://www.weblio.jp/content_find?query={term}"
        >在 Weblio 搜尋「{term}」</Out
      >
    </li>
  </ul>
</div>
