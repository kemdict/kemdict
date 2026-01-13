<script lang="ts">
  import type { Heteronym } from "common";
  import type { OutputWord } from "$dicts/ministry-of-education/kautian.ts";
  let { heteronyms }: { heteronyms: Array<Heteronym<OutputWord>> } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
  import { groupByProp } from "common";
</script>

{#each heteronyms as word}
  <h1 id="kautian-word-{word.props.id}">{word.title}</h1>
  <Pronunciation>{word.props.tl.main}</Pronunciation>
  <Property key="異用字" value={word.props.han.alt?.join("、")}></Property>
  <!-- <div>Tl alt: {JSON.stringify(word.props.tl)}</div> -->
  {#each groupByProp(word.props.heteronyms, "pos", "none") as [pos, hets]}
    {#if pos !== "none"}
      <p class="pos">{pos}</p>
    {/if}
    <ol>
      {#each hets as het}
        <li id="kautian-het-{het.id}">
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
