<script>
  export let heteronyms;
  export let title;
  import { spc } from "$lib/processing";
  import Pronunciation from "$lib/Pronunciation.svelte";

  function split(str) {
    // Some lines contain just a Tab. Get rid of them.
    return str.split("\n").filter((x) => x.trim().length > 0);
  }
</script>

{#each heteronyms as het}
  <h1>{title}</h1>
  {#if het.props.bopomofo}
    <Pronunciation>{spc(het.props.bopomofo)}</Pronunciation>
  {/if}
  {#if het.props.definition}
    <ol>
      {#each split(het.props.definition) as d}
        <!-- TODO: we really need a better way of displaying nested lists. -->
        <li><p class="def">{@html d.replace(/　(\(\d+\))/g, "<br>$1")}</p></li>
      {/each}
    </ol>
  {/if}
  {#if het.props.antonyms}
    {@html het.props.antonyms}
  {/if}
  {#if het.props.synonyms}
    {@html het.props.synonyms}
  {/if}
{/each}
