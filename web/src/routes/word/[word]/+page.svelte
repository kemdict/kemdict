<script>
  /** @type {import('./$types').PageData} */
  export let data;

  import Elsewhere from "$lib/Elsewhere.svelte";
  import Out from "$lib/Out.svelte";
  import SplitLayout from "$lib/SplitLayout.svelte";
  import TOC from "$lib/TOC.svelte";
  import Word from "$lib/Word.svelte";
  import WordList from "$lib/WordList.svelte";

  import { dictsInWord } from "$lib/common";

  $: word = data.word;
  $: backlinks = data.backlinks;
  $: presentDicts = dictsInWord(word);
</script>

<svelte:head>
  <title>「{word.title}」 - kemdict</title>
  <meta name="description" content="「{word.title}」的定義" />
</svelte:head>

<SplitLayout leftFirst={false} initialInput={word.title}>
  <div slot="left">
    <div class="not-sm:hidden">
      <TOC {presentDicts} />
    </div>
    <Elsewhere term={word.title} />
    {#if backlinks.length > 0}
      <div class="prose">
        <h2>有提到「{word.title}」的條目</h2>
        <WordList words={backlinks} />
      </div>
    {/if}
  </div>
  <svelte:fragment slot="right">
    <div class="sm:hidden"><TOC term={word.title} {presentDicts} /></div>
    <Word {word} />
  </svelte:fragment>
</SplitLayout>

<div class="prose mt-8">
  {#if presentDicts.length > 1 || presentDicts[0] !== "kisaragi_dict"}
    <h2 class="mt-2">本頁的著作權</h2>
    <p>
      《重編國語辭典修訂本》、《國語辭典簡編本》、《成語典》、《臺灣客家語常用詞辭典》與《臺灣閩南語常用詞辭典》為中華民國教育部版權所有，依「創用CC-姓名標示-禁止改作
      3.0 臺灣授權條款」釋出。
    </p>
    {#if word.dict_concised}
      <p>
        《<Out href="https://dict.concised.moe.edu.tw">國語辭典簡編本</Out>》©
        中華民國教育部（版本編號：2014_20220928，<a
          href="/l/concisedict_10312.pdf">使用說明</a
        >）
      </p>
    {/if}
    {#if word.dict_revised}
      <p>
        《<Out href="https://dict.revised.moe.edu.tw">重編國語辭典修訂本</Out
        >》© 中華民國教育部（版本編號：2015_20220922，<a
          href="/l/reviseddict_10312.pdf">使用說明</a
        >）
      </p>
    {/if}
    {#if word.moedict_twblg}
      <p>
        《<Out href="https://twblg.dict.edu.tw">臺灣閩南語常用詞辭典</Out>》©
        中華民國教育部（版本編號：20190610，見
        <Out href="https://github.com/g0v/moedict-data-twblg"
          >moedict-data-twblg</Out
        >）
      </p>
    {/if}
    {#if word.hakkadict}
      <p>
        《<Out href="https://hakkadict.moe.edu.tw">臺灣客家語常用詞辭典</Out>》©
        中華民國教育部（版本編號：1100429）
      </p>
    {/if}
    {#if word.dict_idioms}
      <p>
        《<Out href="https://dict.idioms.moe.edu.tw">成語典</Out>》©
        中華民國教育部（版本編號：2020_20220928，<a
          href="/l/idiomsdict_10409.pdf">使用說明</a
        >）
      </p>
    {/if}
  {/if}
  <hr />
  <p class="prose">
    如果 Kemdict 有幫助到您，請考慮<Out
      href="https://www.buymeacoffee.com/kisaragihiu">贊助我一餐飯</Out
    >。謝謝。
  </p>
</div>
