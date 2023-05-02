<script>
  export let heteronyms;
  import Pronunciation from "$src/components/Pronunciation.svelte";

  const p_names = ["四縣", "海陸", "大埔", "饒平", "詔安", "南四縣"];
  function process_type(typ) {
    if (typ) {
      return typ.replace("\n", "/");
    }
  }
  function processDef(d) {
    return d
      .replace(/^\d+\./, "")
      .replace(/例：(.*?)（(.*?)）/, `<blockquote>$1<br>$2</blockquote>`);
  }
  function processDefs(def) {
    if (def) {
      let x = "";
      for (const d of def.split("\n")) {
        if (x.length == 0) {
          x += `<ol>`;
        }
        x += "<li>";
        x += `<p class="def">${processDef(d)}</p>`;
        x += "</li>";
      }
      x += "</ol>";
      return x;
    } else {
      return "";
    }
  }
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  {#each p_names as p}
    {#if het.props[`p_${p}`]}
      <Pronunciation>{p}：{het.props[`p_${p}`]}</Pronunciation>
    {/if}
  {/each}
  {#if het.props.type}
    <p class="pos">{process_type(het.props.type)}</p>
  {/if}
  {@html processDefs(het.props.definition)}
  {#if het.props.antonyms}
    <p><m>反義詞</m>：{@html het.props.antonyms}</p>
  {/if}
  {#if het.props.synonyms}
    <p><m>近義詞</m>：{@html het.props.synonyms}</p>
  {/if}
{/each}
