<script>
  import {
    spc,
    groupByProp,
    interlinear_annotation,
    linkify_brackets,
    linkToWord,
  } from "$lib/common";
  export let word;
  export let dict;
  const entry = word[dict];
  const pronunciation_key_mapping = {
    kisaragi_dict: "pronunciation",
    moedict_twblg: "trs",
  };
  const pronunciation_key = pronunciation_key_mapping[dict];

  function process_def_kisaragi(def) {
    if (def) {
      def = def.replace(/<(.*?)>/g, (_m, $1) => `${linkToWord($1)}`);
      def = linkify_brackets(def);
    } else {
      def = "";
    }
    return def;
  }
</script>

{#each entry.heteronyms as het}
  <h1>{word.title}</h1>
  {#if het[pronunciation_key]}
    <p>讀音：<span>{spc(het[pronunciation_key])}</span></p>
  {/if}
  {#each Object.entries(groupByProp(het.definitions, "type", "none")) as [type, defs]}
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
                <p class="def">{def.def}</p>
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
