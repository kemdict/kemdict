<script lang="ts">
  import Out from "$src/components/Out.svelte";
  import WordDictConcised from "./WordDictConcised.svelte";
  import WordDictIdioms from "./WordDictIdioms.svelte";
  import WordDictRevised from "./WordDictRevised.svelte";
  import WordHakkadict from "./WordHakkadict.svelte";
  import WordITaigi from "./WordITaigi.svelte";
  import WordTaiJit from "./WordTaiJit.svelte";
  import WordKichhooGiku from "./WordKichhooGiku.svelte";
  import WordMaryknoll from "./WordMaryknoll.svelte";
  import WordPtsTaigitv from "./WordPtsTaigitv.svelte";
  import WordMoedictish from "./WordMoedictish.svelte";
  import WordUnihan from "./WordUnihan.svelte";
  import WordILRDF from "./WordILRDF.svelte";
  import WordLopof from "./WordLopof.svelte";

  import { format } from "common";

  import type { Dict, Heteronym } from "common";

  export let groupedHets: [Dict, Heteronym[]][];
  export let title: string;

    if (title === "演算法")
    console.log(groupedHets)
</script>

{#each groupedHets as [dict, hets]}
  <div id={dict.id} class="dict">
    <span
      ><a
        class="hover:link dark:gray-500 text-gray-400"
        href="#{dict.id}"
        id={dict.id}>#</a
      ></span
    >
    <span>
      {#if ["hakkadict", "pts-taigitv", "chhoetaigi_taijittoasutian", "chhoetaigi_taioanpehoekichhoogiku", "chhoetaigi_maryknoll1976"].includes(dict.id)}
        <!-- FIXME: this only links to the first one. -->
        <Out href={format(dict.url, hets[0].props.id)}>{dict.name}</Out>
      {:else if dict.id.startsWith("lopof")}
        <Out href={format(dict.url, `${hets[0].props.page}`)}>{dict.name}</Out>
      {:else}
        <Out href={format(dict.url, title)}>{dict.name}</Out>
      {/if}
    </span>
  </div>
  <div class="word">
    {#if dict.id == "unihan"}
      <WordUnihan heteronyms={hets} />
      <div class="copyright">
        <p>
          {dict.name}
          <a href="https://www.unicode.org/license.txt">© Unicode, Inc.</a>
        </p>
      </div>
    {:else if dict.id === "kisaragi_dict" || dict.id === "kisaragi_taigi"}
      <WordMoedictish heteronyms={hets} dict={dict.id} />
      <div class="copyright">
        <p>
          《{dict.name}》採 CC0 釋出，可無條件隨意複製，隨意利用。
        </p>
        <p>
          這個定義有問題嗎？<a href="https://github.com/kemdict/kemdict/issues"
            >回報問題</a
          >
        </p>
      </div>
    {:else if dict.id == "chhoetaigi_maryknoll1976"}
      <WordMaryknoll heteronyms={hets} />
      <div class="copyright">
        <p>
          《{dict.name}》
        </p>
        <p>原作者：Maryknoll Language Service Center</p>
        <p>
          <a href={dict?.meta?.license?.url}
            >以 姓名標示-Sio-kâng方式分享 3.0 國際 (CC BY-SA 4.0) 授權</a
          >
        </p>
        <p>
          資料來自 <a href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase"
            >ChhoeTaigi 的字詞資料庫</a
          >
        </p>
      </div>
    {:else if dict.id == "chhoetaigi_taioanpehoekichhoogiku"}
      <WordKichhooGiku heteronyms={hets} />
      <div class="copyright">
        <p>
          《{dict.name}》
        </p>
        <p>原作者：Ko Chek-hoàn（高積煥）、Tân Pang-tìn（陳邦鎮）</p>
        <p>
          數位化：<a href="http://ip194097.ntcu.edu.tw/memory/TGB/"
            >台語文記憶</a
          >
        </p>
        <p>
          數位化與編修：Lîm Bûn-cheng、Tēⁿ Tì-têng、Tân Kim-hoa、Chiúⁿ Ji̍t-êng
        </p>
        <p>
          <a
            href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase#8-1956-台灣白話基礎語句"
            >以 姓名標示-Sio-kâng方式分享 4.0 國際 (CC BY-SA 4.0) 授權</a
          >
        </p>
        <p>
          資料來自 <a href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase"
            >ChhoeTaigi 的字詞資料庫</a
          >
        </p>
      </div>
    {:else if dict.id == "chhoetaigi_taijittoasutian"}
      <WordTaiJit heteronyms={hets} />
      <div class="copyright">
        <p>
          《{dict.name}》
        </p>
        <p>原作者：{dict.meta.author}</p>
        <p>台文翻譯kap編修：Lîm Chùn-io̍k（林俊育）長老</p>
        <p>
          <a
            href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase#2-1932-台日大辭典台譯版"
            >以 姓名標示-非商業性-Sio-kâng方式分享 3.0 台灣 (CC BY-NC-SA 3.0 TW)
            授權</a
          >
        </p>
        <p>
          資料來自 <a href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase"
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
    {:else if dict.id == "pts-taigitv"}
      <WordPtsTaigitv heteronyms={hets} />
      <div class="copyright">
        <p>
          <a href="https://taigiwords.taigitv.org.tw">公視《台語新詞辭庫》</a>以
          <a href="https://creativecommons.org/licenses/by/4.0/deed.zh-hant">CC BY 4.0</a>
          條款釋出。
        </p>
      </div>
    {:else if dict.id == "chhoetaigi_itaigi"}
      <WordITaigi heteronyms={hets} />
      <div class="copyright">
        <p>
          《{dict.name}》資料取自
          <a href="https://itaigi.tw/">iTaigi</a>，採
          <a href="https://itaigi.tw/hokbu">CC0</a> 釋出。
        </p>
        <p>
          資料來自 <a href="https://github.com/ChhoeTaigi/ChhoeTaigiDatabase"
            >ChhoeTaigi 的字詞資料庫</a
          >。
        </p>
      </div>
    {:else if dict.id == "dict_revised"}
      <WordDictRevised heteronyms={hets} />
      <div class="copyright">
        <p>
          《<a href="https://dict.revised.moe.edu.tw/">重編國語辭典修訂本</a
          >》版本編號：{dict.meta.version}
        </p>
        <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
        <p><a href={dict.meta.license.url}>使用說明</a></p>
      </div>
    {:else if dict.id == "dict_concised"}
      <WordDictConcised heteronyms={hets} />
      <div class="copyright">
        <p>
          《<a href="https://dict.concised.moe.edu.tw">國語辭典簡編本</a
          >》版本編號：{dict.meta.version}
        </p>
        <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
        <p><a href={dict.meta.license.url}>使用說明</a></p>
      </div>
    {:else if dict.id == "moedict_twblg"}
      <WordMoedictish heteronyms={hets} dict={dict.id} />
      <div class="copyright">
        <p>
          《<a href={dict.meta.original}>臺灣閩南語常用詞辭典</a
          >》版本編號：{dict.meta.version}
        </p>
        <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
        <p>
          資料來自<a href="https://github.com/g0v/moedict-data-twblg"
            >moedict-data-twblg</a
          >
        </p>
      </div>
    {:else if dict.id == "hakkadict"}
      <WordHakkadict heteronyms={hets} />
      <div class="copyright">
        <p>
          《<a href="https://hakkadict.moe.edu.tw">臺灣客家語常用詞辭典</a
          >》版本編號：{dict.meta.version}
        </p>
        <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
      </div>
    {:else if dict.id == "dict_idioms"}
      <WordDictIdioms heteronyms={hets} />
      <div class="copyright">
        <p>
          《<a href="https://dict.idioms.moe.edu.tw">成語典</a>》版本編號：{dict
            .meta.version}
        </p>
        <p>© 中華民國教育部 (Ministry of Education, R.O.C.)</p>
        <p><a href={dict.meta.license.url}>使用說明</a></p>
      </div>
    {:else if dict.id.startsWith("lopof")}
      <WordLopof heteronyms={hets} />
    {:else if dict.id.startsWith("ilrdf")}
      {@const lang = dict.id.slice(-3)}
      <WordILRDF heteronyms={hets} {lang} />
    {/if}
  </div>
{/each}
