<script>
  export let heteronyms;
  import Pronunciation from "$src/components/Pronunciation.svelte";
  function getTitle(het) {
    if (het.props.title === het.props.HanLoTaibunKip) {
      return het.props.title;
    } else {
      return `${het.props.title} (${het.props.HanLoTaibunKip})`;
    }
  }
  function process_def(def) {
    if (def) {
      let x = "<ol>";
      for (const d of def.split(/\n/)) {
        x += `<li><p class="def">${d.replace(/\(\d+\)/, "")}</p></li>`;
      }
      x += "</ol>";
      return x;
    }
  }
  function splitExample(ex) {
    if (ex) {
      return (
        ex
          // The "(1)" is a marker for which definition to apply to.
          // Some examples are delimited by this marker alone, without
          // the semicolon. (See 戶 for an example.)
          // Insert a semicolon so it's easier to split.
          // TODO: put the examples between the definitions instead
          .replace(/([^；]\(\d+\))/, "；$1")
          .split("；")
      );
    } else {
      return [];
    }
  }
</script>

{#each heteronyms as het}
  <h1>{getTitle(het)}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  {#if het.props.definition}
    <p class="my-2">{@html process_def(het.props.definition)}</p>
    {#if het.props.example}
      <blockquote>
        <ul class="list-disc pl-2">
          {#each splitExample(het.props.example) as ex}
            <li>{ex}</li>
          {/each}
        </ul>
      </blockquote>
    {/if}
  {:else}
    <p class="my-2">{@html process_def(het.props.example)}</p>
  {/if}
{/each}
