<script>
  export let heteronyms;
  export let title;
  import Pronunciation from "$lib/Pronunciation.svelte";

  const p_names = ["四縣", "海陸", "大埔", "饒平", "詔安", "南四縣"];

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
      return x;
    } else {
      return "";
    }
  }
</script>

{#each heteronyms as het}
  <h1>{title}</h1>
  {#each p_names as p}
    {#if het.props[`p_${p}`]}
      <Pronunciation>{p}：{het.props[`p_${p}`]}</Pronunciation>
    {/if}
  {/each}
  {@html process_def(het.props.definition)}
{/each}
