<script>
  export let entry;
  export let title;
  import { spc, linkToWord, linkify_brackets } from "$lib/common";

  function split(str) {
    // Some lines contain just a Tab. Get rid of them.
    return str.split("\n").filter((x) => x.trim().length > 0);
  }

  // This only processes one item in the definitions.
  function process_def(d) {
    d = d
      // This means "this definition has an image".
      .replace("　◎", "")
      .replace(/^\d+\./, "")
      .replace(/(△|]|、)([^、。]+)/g, (_m, $1, $2) => {
        return `${$1}${linkToWord($2)}`;
      })
      // These are the only types that exist.
      // ...plus CJK COMPATIBILITY IDEOGRAPH-F9B5. (Fixed in upstream
      // already, should be available next time concised dict makes a
      // data release.)
      .replace(/\[([例似反])\]/g, `<br><m>$1</m>`)
      .replace(/§(英)([a-zA-Z ]+)/g, `<br><m>$1</m>$2`)
      .replace("△", `<br><m title="參考詞">△</m>`);
    d = linkify_brackets(d);
    return d;
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
        <li><p class="def">{@html process_def(d)}</p></li>
      {/each}
    </ol>
  {/if}
  {#if het.antonyms}
    {@html process_def(het.antonyms)}
  {/if}
  {#if het.synonyms}
    {@html process_def(het.synonyms)}
  {/if}
{/each}
