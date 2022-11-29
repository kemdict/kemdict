<script>
  import WordDictConcised from "$lib/components/WordDictConcised.svelte";
  import WordDictIdioms from "$lib/components/WordDictIdioms.svelte";
  import WordDictRevised from "$lib/components/WordDictRevised.svelte";
  import WordHakkadict from "$lib/components/WordHakkadict.svelte";
  import WordMoedictish from "$lib/components/WordMoedictish.svelte";
  import { dicts } from "$lib/common.js";
  export let word;
  export let dict;
  // This allows us to also pass in which dictionary we want and only
  // render for it.
  let dictionaries;
  if (dict) {
    dictionaries = [dict];
  } else {
    dictionaries = Object.keys(dicts);
  }
</script>

{#each dictionaries as dict}
  <div>
    {#if word[dict]}
      <div id={dict} class="dict">
        {#if dict == "kisaragi_dict"}
          <a href="/dict-kisaragi">如月的現代台灣華語補足典</a>
        {:else if dict == "dict_idioms"}
          <a
            href="https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom={word.title}"
            >教育部成語典</a
          >
        {:else if dict == "moedict_twblg"}
          <a
            href="https://twblg.dict.edu.tw/holodict_new/result_main.jsp?radiobutton=1&limit=20&querytarget=1&sample={word.title}"
            >教育部臺灣閩南語常用詞辭典</a
          >
        {:else if dict == "hakkadict"}
          <a
            href="https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi/ccd=qwMPHD/search?dcf=sti&extrasearch=es1&qs0={word.title}"
            >教育部臺灣客家語常用詞辭典</a
          >
        {:else if dict == "dict_concised"}
          <a
            href="https://dict.concised.moe.edu.tw/search.jsp?word={word.title}"
            >教育部國語辭典簡編本</a
          >
        {:else if dict == "dict_revised"}
          <a href="https://dict.revised.moe.edu.tw/search.jsp?word={word.title}"
            >教育部重編國語辭典</a
          >
        {/if}
      </div>
      <div class="word">
        {#if dict == "kisaragi_dict"}
          <WordMoedictish {word} {dict} />
        {:else if dict == "dict_revised"}
          <WordDictRevised entry={word[dict]} title={word.title} />
        {:else if dict == "dict_concised"}
          <WordDictConcised entry={word[dict]} title={word.title} />
        {:else if dict == "moedict_twblg"}
          <WordMoedictish {word} {dict} />
        {:else if dict == "hakkadict"}
          <WordHakkadict entry={word[dict]} title={word.title} />
        {:else if dict == "dict_idioms"}
          <WordDictIdioms entry={word[dict]} title={word.title} />
        {/if}
      </div>
    {/if}
  </div>
{/each}
