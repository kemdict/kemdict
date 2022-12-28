<script>
  export let entry;
  export let title;
  import { spc } from "$lib/processing";

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

{#each entry.heteronyms as het}
  <h1>{title}</h1>
  {#if het.bopomofo}
    <p>讀音：<span>{spc(het.bopomofo)}</span></p>
  {/if}
  {@html process_def(het.definition)}
{/each}
