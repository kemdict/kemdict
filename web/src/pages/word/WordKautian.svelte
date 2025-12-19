<script lang="ts">
  import type { Heteronym } from "common";

  export let heteronyms: Heteronym[];

  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { groupByProp } from "common";
  import { spc, interlinear_annotation } from "$lib/processing";

  function process_def_kisaragi(def: string | undefined) {
    if (def) {
      def = def.replace(/([^>])\n+/g, "$1<br/>");
    } else {
      def = "";
    }
    return def;
  }
</script>

{JSON.stringify(groupByProp(heteronyms, (het) => het.props.pos))}

<h1>{heteronyms[0].title}</h1>

{#each groupByProp(heteronyms, (het) => het.props.pos, "none") as [pos, hets]}
  {#if pos !== "none"}
    <p class="pos">{pos}</p>
  {/if}
  <ol>
    {#each hets as het}
      {#if het.props.tl && het.props.tl !== het.title}
        <Pronunciation>{spc(het.props.tl)}</Pronunciation>
      {/if}
      <li>
        <p class="def">
          {het.props.def}
        </p>
        {#each het.props.examples as example}
          <blockquote>
            <p>{example.han}</p>
            <p>{example.tl}</p>
            <p class="pt-1 opacity-80">({example.zh})</p>
          </blockquote>
        {/each}
      </li>
    {/each}
  </ol>
{/each}
