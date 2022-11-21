<script>
  import { page } from "$app/stores";
  import SearchBar from "$lib/components/SearchBar.svelte";
  import Header from "$lib/components/Header.svelte";
  const term = $page.params.word;
</script>

<svelte:head>
  <title>找不到「{term}」 - Kemdict</title>
</svelte:head>

<Header>
  <SearchBar />
</Header>

<div class="prose bullets">
  <h1>{term}</h1>
  <p>Kemdict 收錄字典裡找不到這個詞。</p>
  <p />
  <ul class="my-10">
    <li>
      <a href="https://google.com/search?q={term}">在 Google 搜尋「{term}」</a>
    </li>
    <li>
      <a href="https://zh.wiktionary.org/w/index.php?search={term}"
        >在維基詞典搜尋「{term}」</a
      >
    </li>
    <li>
      <a
        href="https://terms.naer.edu.tw/search/?match_type=phrase&query_op=&query_field=title&query_term={term}"
        >在學術名詞資料庫搜尋「{term}」</a
      >
    </li>
    <li>
      <a href="https://www.weblio.jp/content_find?query={term}"
        >在 Weblio 搜尋「{term}」</a
      >
    </li>
  </ul>
</div>
