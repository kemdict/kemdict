<script>
  export let entry;
  import Pronunciation from "$lib/Pronunciation.svelte";
  function getTitle(het) {
    if (het.title === het.HanLoTaibunKip) {
      return het.title;
    } else {
      return `${het.title} (${het.HanLoTaibunKip})`;
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
</script>

{#each entry.heteronyms as het}
  <h1>{getTitle(het)}</h1>
  <Pronunciation>{het.kip}</Pronunciation>
  <p class="my-2">{@html process_def(het.definition)}</p>
  <blockquote>
    <ul class="list-disc pl-2">
      {#each het.example.split("；") as ex}
        <li>{ex}</li>
      {/each}
    </ul>
  </blockquote>
{/each}
