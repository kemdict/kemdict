/**
 * Render the 404 page according to the attempted search term.
 * @file render-not-found.ts
 */

function renderNotFound(term: string): string {
  return `
<h1>${term}</h1>
<p>Kemdict 收錄字典裡找不到這個詞。<p>
<ul class="my-10">
  <li>
    <a href="https://google.com/search?q=${term}"
      >在 Google 搜尋「${term}」</a
    >
  </li>
  <li>
    <a href="https://zh.wiktionary.org/w/index.php?search=${term}"
      >在維基詞典搜尋「${term}」</a
    >
  </li>
  <li>
    <a
      href="https://terms.naer.edu.tw/search/?match_type=phrase&query_op=&query_field=title&query_term=${term}"
      >在學術名詞資料庫搜尋「${term}」</a
    >
  </li>
  <li>
    <a href="https://www.weblio.jp/content_find?query=${term}"
      >在 Weblio 搜尋「${term}」</a
    >
  </li>
</ul>
`;
}

let term = decodeURI(location.href).match(/\/word\/(.*)\/?/);
if (term) {
  let body = document.getElementsByClassName("body")[0];
  let container = document.createElement("div");
  container.classList.add("prose");
  container.innerHTML = renderNotFound(term[1]);
  body.appendChild(container);
}
