<script>
  export let heteronyms;
  import { spc } from "$src/processing";
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function split(str) {
    // Some lines contain just a Tab. Get rid of them.
    return str.split("\n").filter((x) => x.trim().length > 0);
  }
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
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
    <p>{@html het.props.antonyms}</p>
  {/if}
  {#if het.props.synonyms}
    <p>{@html het.props.synonyms}</p>
  {/if}
{/each}
