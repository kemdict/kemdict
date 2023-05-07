<script lang="ts">
  import type { Heteronym } from "common";
  interface HetRevised extends Heteronym {
    props: {
      title: string;
      alias: string;
      length: string;
      id: string;
      radical: string;
      sc: string;
      nrsc: string;
      het_sort: string;
      bopomofo: string;
      v_type: string;
      v_bopomofo: string;
      pinyin: string;
      v_pinyin: string;
      synonyms: string;
      antonyms: string;
      def: string;
      het_ref: string;
      異體字: string;
    };
  }
  export let heteronyms: HetRevised[];
  import { spc, radicals_and_strokes } from "$src/processing";
  import { strLen } from "common";
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function process_def(def: string): string {
    if (def) {
      let x = "";
      let match: RegExpMatchArray;
      for (const d of def.split("\n")) {
        match = d.match(/^\[(.*)\]$/);
        if (match && match[1]) {
          if (x.length > 0) {
            x += `</ol>`;
          }
          x += `<p class="pos">${match[1]}</p><ol>`;
        } else {
          if (x.length == 0) {
            x += `<ol>`;
          }
          x += `<li><p class="def">${d.replace(/^\d+\./, "")}</p></li>`;
        }
      }
      x += "</ol>";
      return x.replace(/(\(\d+\))/g, "<br>$1");
    } else {
      return "";
    }
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
  {@html process_def(het.props.def)}
{/each}
