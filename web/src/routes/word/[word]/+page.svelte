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
  import WordTaiJit from "$lib/WordTaiJit.svelte";
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
  <div class="sm:fixed" slot="left">
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
          {#if dict.id === "chhoetaigi_taijittoasutian"}
            <Out href={format(dict.url, word[dict.id].heteronyms[0].het_sort)}
              >{dict.name}</Out
            >
          {:else}
            <Out href={format(dict.url, word.title)}>{dict.name}</Out>
          {/if}
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
            <div class="copyright">
              <p>《{dict.name}》採 CC0 釋出，可無條件隨意複製，隨意利用。</p>
              <p>
                這個定義有問題嗎？<a
                  href="https://github.com/kemdict/kemdict-data/issues"
                  >回報問題</a
                >
              </p>
            </div>
          {:else if dict.id == "chhoetaigi_taijittoasutian"}
            <WordTaiJit entry={word[dict.id]} />
            <div class="copyright">
              <p>
                《{dict.name}》
              </p>
              <p>原作者：小川尚義</p>
              <p>台文翻譯kap編修：Lîm Chùn-io̍k（林俊育）長老</p>
              <p>
                <a
                  href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase#2-1932-台日大辭典台譯版"
                  >以 姓名標示-非商業性-Sio-kâng方式分享 3.0 台灣 (CC BY-NC-SA
                  3.0 TW) 授權</a
                >
              </p>
              <p>
                資料來自 <a
                  href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase"
                  >ChhoeTaigi 的字詞資料庫</a
                >
              </p>
              <p>
                <a href="https://taigi.fhl.net/dict/">台文版原始網站</a>；<a
                  href="https://github.com/fhl-net/Lim-Chun-iok_2008_Tai-jip-Tua-su-tian"
                  >台文版原 GitHub 儲存庫</a
                >
              </p>
            </div>
          {:else if dict.id == "chhoetaigi_itaigi"}
            <WordITaigi entry={word[dict.id]} />
            <div class="copyright">
              <p>
                《{dict.name}》資料取自
                <a href="https://itaigi.tw/">iTaigi</a>，採
                <a href="https://itaigi.tw/hokbu">CC0</a> 釋出。
              </p>
              <p>
                資料來自 <a
                  href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase"
                  >ChhoeTaigi 的字詞資料庫</a
                >。
              </p>
            </div>
          {:else if dict.id == "dict_revised"}
            <WordDictRevised entry={word[dict.id]} title={word.title} />
            <div class="copyright">
              <p>
                《<a href="https://dict.revised.moe.edu.tw/"
                  >重編國語辭典修訂本</a
                >》版本編號：2015_20220922
              </p>
              <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
              <p><a href="/l/reviseddict_10312.pdf">使用說明</a></p>
            </div>
          {:else if dict.id == "dict_concised"}
            <WordDictConcised entry={word[dict.id]} title={word.title} />
            <div class="copyright">
              <p>
                《<a href="https://dict.concised.moe.edu.tw">國語辭典簡編本</a
                >》版本編號：2014_20220928
              </p>
              <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
              <p><a href="/l/conciseddict_10312.pdf">使用說明</a></p>
            </div>
          {:else if dict.id == "moedict_twblg"}
            <WordMoedictish {word} dict={dict.id} />
            <div class="copyright">
              <p>
                《<a href="https://twblg.dict.edu.tw">臺灣閩南語常用詞辭典</a
                >》版本編號：20190610
              </p>
              <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
              <p>
                資料來自<a href="https://github.com/g0v/moedict-data-twblg"
                  >moedict-data-twblg</a
                >
              </p>
            </div>
          {:else if dict.id == "hakkadict"}
            <WordHakkadict entry={word[dict.id]} title={word.title} />
            <div class="copyright">
              <p>
                《<a href="https://hakkadict.moe.edu.tw">臺灣客家語常用詞辭典</a
                >》版本編號：1100429
              </p>
              <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
            </div>
          {:else if dict.id == "dict_idioms"}
            <WordDictIdioms entry={word[dict.id]} title={word.title} />
            <div class="copyright">
              <p>
                《<a href="https://dict.idioms.moe.edu.tw">成語典</a
                >》版本編號：2020_20220928
              </p>
              <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
              <p><a href="/l/idiomsdict_10409.pdf">使用說明</a></p>
            </div>
          {/if}
        </div>
      {/each}
    {/each}
  </svelte:fragment>
</SplitLayout>
