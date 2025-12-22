<script lang="ts">
  import type { Heteronym } from "common";
  export let heteronyms: Heteronym[];
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { groupByProp } from "common";
</script>

{#each heteronyms as word}
  <h1>{word.title}</h1>
  <Pronunciation>{word.props.tl.main}</Pronunciation>
  <div>異用字：{word.props.han.alt?.join("、")}</div>
  <!-- <div>Tl alt: {JSON.stringify(word.props.tl)}</div> -->
  {#each groupByProp(word.props.heteronyms, "pos") as [pos, hets]}
    {#if pos !== "none"}
      <p class="pos">{pos}</p>
    {/if}
    <ol>
      {#each hets as het}
        <li>
          <p class="def">
            {het.def}
          </p>
          {#each het.examples as example}
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
{/each}
