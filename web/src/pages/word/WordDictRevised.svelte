<script>
  export let heteronyms;
  import { spc, radicals_and_strokes } from "$src/processing";
  import { strLen } from "$src/common";
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function process_def(def) {
    if (def) {
      let x = "";
      let match;
      for (const d of def.split("\n")) {
        if ((match = d.match(/^\[(.*)\]$/))) {
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
  {@html process_def(het.props.definition)}
{/each}
