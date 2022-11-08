<script>
  /** @type {import('./$types').PageData} */
  import Word from "$lib/components/Word.svelte";
  import SearchBar from "$lib/components/SearchBar.svelte";
  export let data;
  $: word = data.word;
</script>

<svelte:head>
  <title>「{word.title}」 - kemdict</title>
</svelte:head>

<div class="h">
  <h1><a href="/">kemdict (β)</a></h1>
  <SearchBar />
</div>

{#if word.dict_revised}
  <Word {word} dict="dict_revised" />
{/if}
{#if word.dict_concised}
  <Word {word} dict="dict_concised" />
{/if}
{#if word.moedict_twblg}
  <Word {word} dict="moedict_twblg" />
{/if}
{#if word.dict_idioms}
  <Word {word} dict="dict_idioms" />
{/if}
{#if word.kisaragi_dict}
  <Word {word} dict="kisaragi_dict" />
{/if}

<hr />
<div class="prose">
  <ul class="my-10">
    <li>
      <a href="https://google.com/search?q={word.title}">在 Google 搜尋「{word.title}」</a>
    </li>
    <li>
      <a href="https://zh.wiktionary.org/w/index.php?search={word.title}"
        >在維基詞典搜尋「{word.title}」</a
      >
    </li>
    <li>
      <a href="https://moedict.tw/{word.title}">在萌典搜尋「{word.title}」</a>
    </li>
    <li>
      <a
        href="https://terms.naer.edu.tw/search/?match_type=phrase&query_op=&query_field=title&query_term={word.title}"
        >在學術名詞資料庫搜尋「{word.title}」</a
      >
    </li>
    <li>
      <a href="https://www.weblio.jp/content_find?query={word.title}"
        >在 Weblio 搜尋「{word.title}」</a
      >
    </li>
  </ul>
  {#if word.dict_revised || word.moedict_twblg || word.dict_concised || word.dict_idioms}
    <h2 class="mt-2">本頁的著作權</h2>
    <p>
      中華民國教育部《重編國語辭典修訂本》、《國語辭典簡編本》、《國語小字典》與《成語典》相關資料採「創用CC-姓名標示-禁止改作
      3.0 臺灣授權條款」釋出。
    </p>
    {#if word.dict_revised}
      <p>
        《<a href="https://dict.revised.moe.edu.tw">重編國語辭典修訂本</a>》©
        中華民國教育部（版本編號：2015_20220922）。<a href="/l/reviseddict_10312.pdf">使用說明</a>
      </p>
    {/if}
    {#if word.dict_concised}
      <p>
        《<a href="https://dict.concised.moe.edu.tw">國語辭典簡編本</a>》©
        中華民國教育部（版本編號：2014_20220928）。<a href="/l/concisedict_10312.pdf">使用說明</a>
      </p>
    {/if}
    {#if word.moedict_twblg}
      <p>
        《<a href="https://twblg.dict.edu.tw">臺灣閩南語常用詞辭典</a>》©
        中華民國教育部（版本編號：20190610）。見<a href="https://github.com/g0v/moedict-data-twblg"
          >moedict-data-twblg</a
        >
      </p>
    {/if}
    {#if word.dict_idioms}
      <p>
        《<a href="https://dict.idioms.moe.edu.tw">成語典</a>》©
        中華民國教育部（版本編號：2020_20220928）。<a href="/l/idiomsdict_10409.pdf">使用說明</a>
      </p>
    {/if}
  {/if}
</div>
