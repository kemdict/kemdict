<script>
  export let entry;
  export let title;
  import { spc } from "$lib/processing";

  function split(str) {
    // Some lines contain just a Tab. Get rid of them.
    return str.split("\n").filter((x) => x.trim().length > 0);
  }
</script>

{#each entry.heteronyms as het}
  <h1>{title}</h1>
  {#if het.bopomofo}
    <p>讀音：<span>{spc(het.bopomofo)}</span></p>
  {/if}
  {#if het.definition}
    <ol>
      {#each split(het.definition) as d}
        <!-- TODO: we really need a better way of displaying nested lists. -->
        <li><p class="def">{@html d.replace(/　(\(\d+\))/g, "<br>$1")}</p></li>
      {/each}
    </ol>
  {/if}
  {#if het.antonyms}
    {@html het.antonyms}
  {/if}
  {#if het.synonyms}
    {@html het.synonyms}
  {/if}
{/each}
