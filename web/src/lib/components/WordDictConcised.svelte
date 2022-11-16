<script>
  export let entry;
  export let title;
  import { spc } from "$lib/common";

  // This only processes one item in the definitions.
  function process_def(d) {
    return (
      d
        .replace(/^\d+\./, "")
        // These are the only types that exist.
        // ...plus CJK COMPATIBILITY IDEOGRAPH-F9B5.
        .replace(/\[([例似反])\]/, `<br><m>$1</m>`)
        .replace(/§(英)([a-zA-Z ]+)/, `<br><m>$1</m>$2`)
        .replace("△", `<br><m title="同">△</m>`)
    );
  }
</script>

{#each entry.heteronyms as het}
  <h1>{title}</h1>
  {#if het.bopomofo}
    <p>讀音：<span>{spc(het.bopomofo)}</span></p>
  {/if}
  {#if het.definition}
    <ol>
      {#each het.definition.split("\n") as d}
        <li><p class="def">{@html process_def(d)}</p></li>
      {/each}
    </ol>
  {/if}
  {#if het.antonyms}
    {het.antonyms}
  {/if}
  {#if het.synonyms}
    {het.synonyms}
  {/if}
{/each}
