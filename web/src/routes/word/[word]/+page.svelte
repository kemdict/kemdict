<script>
  /** @type {import('./$types').PageData} */
  import Word from "$lib/components/Word.svelte";
  import WordList from "$lib/components/WordList.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  import Header from "$lib/components/Header.svelte";
  export let data;
  $: word = data.word;
  $: backlinks = data.backlinks;
</script>

<svelte:head>
  <title>「{word.title}」 - kemdict</title>
  <meta name="description" content="「{word.title}」的定義" />
</svelte:head>

<Header>
  <SearchBar initialInput={word.title} />
</Header>

<Word {word} />

{#if backlinks.length > 0}
  <div class="prose">
    <h1>有提到「{word.title}」的條目</h1>
    <WordList words={backlinks} />
  </div>
{/if}

<hr />
<div class="prose">
  <h2>在其他地方搜尋「{word.title}」</h2>
  <ul>
    <li>
      <a href="https://google.com/search?q={word.title}">Google</a>
    </li>
    <li>
      <a href="https://zh.wikipedia.org/w/index.php?search={word.title}"
        >維基百科</a
      >
    </li>
    <li>
      <a href="https://zh.wiktionary.org/w/index.php?search={word.title}"
        >維基詞典</a
      >
    </li>
    <li>
      <a href="https://moedict.tw/{word.title}">萌典</a>
    </li>
    <li>
      <a
        href="https://terms.naer.edu.tw/search/?match_type=phrase&query_op=&query_field=title&query_term={word.title}"
        >學術名詞資料庫</a
      >
    </li>
    <li>
      <a href="https://www.weblio.jp/content_find?query={word.title}">Weblio</a>
    </li>
  </ul>
  {#if word.hakkadict || word.dict_revised || word.moedict_twblg || word.dict_concised || word.dict_idioms}
    <h2 class="mt-2">本頁的著作權</h2>
    <p>
      《重編國語辭典修訂本》、《國語辭典簡編本》、《成語典》、《臺灣客家語常用詞辭典》與《臺灣閩南語常用詞辭典》為中華民國教育部版權所有，依「創用CC-姓名標示-禁止改作
      3.0 臺灣授權條款」釋出。
    </p>
    {#if word.dict_concised}
      <p>
        《<a href="https://dict.concised.moe.edu.tw">國語辭典簡編本</a>》©
        中華民國教育部（版本編號：2014_20220928，<a
          href="/l/concisedict_10312.pdf">使用說明</a
        >）
      </p>
    {/if}
    {#if word.dict_revised}
      <p>
        《<a href="https://dict.revised.moe.edu.tw">重編國語辭典修訂本</a>》©
        中華民國教育部（版本編號：2015_20220922，<a
          href="/l/reviseddict_10312.pdf">使用說明</a
        >）
      </p>
    {/if}
    {#if word.moedict_twblg}
      <p>
        《<a href="https://twblg.dict.edu.tw">臺灣閩南語常用詞辭典</a>》©
        中華民國教育部（版本編號：20190610，見
        <a href="https://github.com/g0v/moedict-data-twblg"
          >moedict-data-twblg</a
        >）
      </p>
    {/if}
    {#if word.hakkadict}
      <p>
        《<a href="https://hakkadict.moe.edu.tw">臺灣客家語常用詞辭典</a>》©
        中華民國教育部（版本編號：1100429）
      </p>
    {/if}
    {#if word.dict_idioms}
      <p>
        《<a href="https://dict.idioms.moe.edu.tw">成語典</a>》©
        中華民國教育部（版本編號：2020_20220928，<a
          href="/l/idiomsdict_10409.pdf">使用說明</a
        >）
      </p>
    {/if}
  {/if}
</div>

<hr />
<p class="prose">
  如果 Kemdict 有幫助到您，請考慮<a
    href="https://www.buymeacoffee.com/kisaragihiu">贊助我一餐飯</a
  >。謝謝。
</p>
