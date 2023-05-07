<script lang="ts">
  import type { Heteronym } from "common";
  interface HetConcised extends Heteronym {
    props: {
      id: string;
      radical: string;
      bopomofo: string;
      def: string;
      synonyms: string;
      antonyms: string;
    };
  }
  export let heteronyms: Array<HetConcised> = [];
  import { spc, radicals_and_strokes } from "$src/processing";
  import { strLen } from "common";
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function split(str: string): string[] {
    // Some lines contain just a Tab. Get rid of them.
    return str.split("\n").filter((x) => x.trim().length > 0);
  }
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  {#if het.props.bopomofo}
    <Pronunciation>{spc(het.props.bopomofo)}</Pronunciation>
  {/if}
  {#if strLen(het.title) === 1}
    {@html radicals_and_strokes(het.props)}
  {/if}
  {#if het.props.def}
    <ol>
      {#each split(het.props.def) as d}
        <!-- TODO: we really need a better way of displaying nested lists. -->
        <li><p class="def">{@html d.replace(/　(\(\d+\))/g, "<br>$1")}</p></li>
      {/each}
    </ol>
  {/if}
  {#if het.props.antonyms}
    <p>{@html het.props.antonyms}</p>
  {/if}
  {#if het.props.synonyms}
    <p>{@html het.props.synonyms}</p>
  {/if}
{/each}
