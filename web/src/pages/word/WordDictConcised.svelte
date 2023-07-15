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
  import { spc, radicals_and_strokes } from "$lib/processing";
  import { strLen } from "common";
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function split(str: string): string[] {
    // Some lines contain just a Tab. Get rid of them.
    return str.split("\n").filter((x) => x.trim().length > 0);
  }

  function processDef(def: string) {
    return (
      def
        // These are the only types that exist.
        .replace(/\[([例似反])\]/g, "<br><m>［$1］</m>")
        .replace(/§(英)([a-zA-Z ]+)/, "<br><m>［$1］</m>$2")
        .replace(/　?△/, '<br><m title="參考詞">［△］</m> ')
    );
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
      {#each split(processDef(het.props.def)) as d}
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
