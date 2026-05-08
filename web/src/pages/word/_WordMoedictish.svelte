<script lang="ts">
  import type { Heteronym } from "common";

  export let heteronyms: Heteronym[];
  export let dict: string;

  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { groupByProp } from "common";
  import { spc, interlinear_annotation } from "$lib/processing";

  const pronunciation_key_mapping = {
    kisaragi_dict: "pronunciation",
    kisaragi_taigi: "pronunciation",
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
      >{spc(het.props[pronunciation_key])}
    </Pronunciation>
  {/if}
  {#if het.props?.tags.length > 0}
    <div class="flex gap-2 flex-wrap">
      {#each het.props.tags as tag}
        <a href={`/search?q=%23${tag}&m=exact`}>#{tag}</a>
      {/each}
    </div>
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
        {/if}
      {/each}
    </ol>
  {/each}
{/each}

{#if heteronyms[0]?.props?.wordTags.length > 0}
  <div class="flex gap-2 flex-wrap">
    {#each het.props.wordTags as tag}
      <a href={`/search?q=%23${tag}&m=exact`}>#{tag}</a>
    {/each}
  </div>
{/if}
