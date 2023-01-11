<script>
  /** @type {import('./$types').PageData} */
  export let data;

  import SplitLayout from "$lib/SplitLayout.svelte";

  import Elsewhere from "$lib/Elsewhere.svelte";
  import Out from "$lib/Out.svelte";
  import TOC from "$lib/TOC.svelte";
  import WordDictConcised from "$lib/WordDictConcised.svelte";
  import WordDictIdioms from "$lib/WordDictIdioms.svelte";
  import WordDictRevised from "$lib/WordDictRevised.svelte";
  import WordHakkadict from "$lib/WordHakkadict.svelte";
  import WordITaigi from "$lib/WordITaigi.svelte";
  import WordList from "$lib/WordList.svelte";
  import WordMoedictish from "$lib/WordMoedictish.svelte";

  import { dictsInWord, format, groupByProp } from "$lib/common";

  $: word = data.word;
  $: backlinks = data.backlinks;
  $: presentDicts = dictsInWord(word, true);
</script>

<svelte:head>
  <title>「{word.title}」 - kemdict</title>
  <meta name="description" content="「{word.title}」的定義" />
</svelte:head>

<SplitLayout leftFirst={false} initialInput={word.title}>
  <div slot="left">
    <div class="max-sm:hidden">
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
    {#each groupByProp(presentDicts, "lang") as [lang, dicts]}
      {#each dicts as dict}
        <div id={dict.id} class="dict">
          <Out href={format(dict.url, word.title)}>{dict.name}</Out>
          <span
            ><a
              class="hover:link dark:gray-500 text-gray-400"
              href="#{dict.id}"
              id={dict.id}>#</a
            ></span
          >
        </div>
        <div class="word">
          {#if dict.id == "kisaragi_dict"}
            <WordMoedictish {word} dict={dict.id} />
          {:else if dict.id == "chhoetaigi_itaigi"}
            <WordITaigi entry={word[dict.id]} />
          {:else if dict.id == "dict_revised"}
            <WordDictRevised entry={word[dict.id]} title={word.title} />
          {:else if dict.id == "dict_concised"}
            <WordDictConcised entry={word[dict.id]} title={word.title} />
          {:else if dict.id == "moedict_twblg"}
            <WordMoedictish {word} dict={dict.id} />
          {:else if dict.id == "hakkadict"}
            <WordHakkadict entry={word[dict.id]} title={word.title} />
          {:else if dict.id == "dict_idioms"}
            <WordDictIdioms entry={word[dict.id]} title={word.title} />
          {/if}
        </div>
      {/each}
    {/each}
  </svelte:fragment>
</SplitLayout>

<div class="prose mt-8">
  {#if presentDicts.length > 1 || presentDicts[0].id !== "kisaragi_dict"}
    <h2 class="mt-2">本頁的著作權</h2>
    <p>
      《重編國語辭典修訂本》、《國語辭典簡編本》、《成語典》、《臺灣客家語常用詞辭典》與《臺灣閩南語常用詞辭典》為中華民國教育部版權所有，依「創用CC-姓名標示-禁止改作
      3.0 臺灣授權條款」釋出。
    </p>
    {#if word.dict_concised}
      <p>
        《<Out href="https://dict.concised.moe.edu.tw">國語辭典簡編本</Out>》©
        中華民國教育部（版本編號：2014_20220928，<a
          href="/l/conciseddict_10312.pdf">使用說明</a
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
