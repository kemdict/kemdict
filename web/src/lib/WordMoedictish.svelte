<script>
  import Pronunciation from "$lib/Pronunciation.svelte";
  import { groupByProp } from "$lib/common";
  import { spc, interlinear_annotation } from "$lib/processing";
  export let word;
  export let dict;

  const pronunciation_key_mapping = {
    kisaragi_dict: "pronunciation",
    moedict_twblg: "trs",
  };

  $: entry = word[dict];
  $: pronunciation_key = pronunciation_key_mapping[dict];

  function process_def_kisaragi(def) {
    if (def) {
      def = def.replace(/\n/g, "<br/>");
    } else {
      def = "";
    }
    return def;
  }
</script>

{#each entry.heteronyms as het}
  <h1>{word.title}</h1>
  {#if het[pronunciation_key] && het[pronunciation_key] !== word.title}
    <Pronunciation
      id={`${dict}--${spc(het[pronunciation_key]).replace(" ", "")}`}
      >{spc(het[pronunciation_key])}</Pronunciation
    >
  {/if}
  {#if entry.vogue || het.vogue}
    <div class="text-gray-700 dark:text-gray-300">（流行語）</div>
  {/if}
  {#each groupByProp(het.definitions, "type", "none") as [type, defs]}
    {#if type !== "none"}
      <p class="pos">{type}</p>
    {/if}
    <ol>
      {#each defs as def}
        {#if dict == "kisaragi_dict"}
          <li>
            <p class="def">
              {@html process_def_kisaragi(def.def)}
              {#if def.example}
                {def.example}
              {/if}
            </p>
            <p>
              {#if def.etymology}
                {@html process_def_kisaragi(def.etymology)}
              {/if}
            </p>
            {#if def.quote}
              {#each def.quote as quote}
                <p>{quote}</p>
              {/each}
            {/if}
          </li>
        {:else if dict == "moedict_twblg"}
          {#if def.def || def.example || def.quote}
            <li>
              {#if def.def}
                <p class="def">{@html def.def}</p>
              {/if}
              {#if def.example}
                <blockquote>
                  {@html interlinear_annotation(def.example)}
                </blockquote>
              {/if}
              {#if def.quote}
                <blockquote>
                  {@html interlinear_annotation(def.quote)}
                </blockquote>
              {/if}
            </li>
          {/if}
        {/if}
      {/each}
    </ol>
  {/each}
{/each}
