<script>
  import WordDictConcised from "./WordDictConcised.svelte";
  import WordDictIdioms from "./WordDictIdioms.svelte";
  import WordDictRevised from "./WordDictRevised.svelte";
  import WordHakkadict from "./WordHakkadict.svelte";
  import WordITaigi from "./WordITaigi.svelte";
  import WordMoedictish from "./WordMoedictish.svelte";
  import Out from "./Out.svelte";
  import { dicts, format } from "$lib/common.js";
  export let word;
</script>

{#each dicts as dict}
  {#if word[dict.id]}
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
  {/if}
{/each}
