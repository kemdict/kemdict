<script lang="ts">
  import type { Heteronym } from "common";

  export let heteronyms: Heteronym[];
  export let dict: string;

  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { groupByProp } from "common";
  import { spc, interlinear_annotation } from "$lib/processing";

  function processPn(pn: string) {
    if (dict === "moedict_twblg") {
      return pn;
    } else {
      return pn;
    }
  }

  const pronunciation_key_mapping = {
    kisaragi_dict: "pronunciation",
    kisaragi_taigi: "pronunciation",
    moedict_twblg: "trs",
  };

  $: pronunciation_key = pronunciation_key_mapping[dict];

  function process_def_kisaragi(def: string | undefined) {
    if (def) {
      def = def.replace(/([^>])\n+/g, "$1<br/>");
    } else {
      def = "";
    }
    return def;
  }
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  {#if het.props[pronunciation_key] && het.props[pronunciation_key] !== het.title}
    <Pronunciation
      id={`${dict}--${spc(het.props[pronunciation_key]).replace(" ", "")}`}
      >{processPn(spc(het.props[pronunciation_key]))}
    </Pronunciation>
  {/if}
  {#if het.props.vogue}
    <div class="text-gray-700 dark:text-gray-300">（流行語）</div>
  {/if}
  {#each groupByProp(het.props.defs, "type", "none") as [type, defs]}
    {#if type !== "none"}
      <p class="pos">{type}</p>
    {/if}
    <ol>
      {#each defs as def}
        {#if dict === "kisaragi_dict" || dict === "kisaragi_taigi"}
          <li>
            <p class="def">
              {@html process_def_kisaragi(def.def)}
            </p>
            {#if def.example}
              <p>
                {def.example}
              </p>
            {/if}
            {#if def.etymology}
              <p>
                {@html process_def_kisaragi(def.etymology)}
              </p>
            {/if}
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
                {@html interlinear_annotation(def.example)}
              {/if}
              {#if def.quote}
                {@html interlinear_annotation(def.quote)}
              {/if}
            </li>
          {/if}
        {/if}
      {/each}
    </ol>
  {/each}
{/each}
