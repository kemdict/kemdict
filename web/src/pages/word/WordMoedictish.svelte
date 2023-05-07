<script>
  export let heteronyms;
  export let dict;

  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { groupByProp } from "common";
  import { toPOJ } from "$src/hack";
  import { spc, interlinear_annotation } from "$src/processing";

  function processPn(pn) {
    if (dict === "moedict_twblg") {
      return toPOJ(pn);
    } else {
      return pn;
    }
  }

  const pronunciation_key_mapping = {
    kisaragi_dict: "pronunciation",
    moedict_twblg: "trs",
  };

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
