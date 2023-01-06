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
</script>

{#each entry.heteronyms as het}
  <h1>{getTitle(het)}</h1>
  <Pronunciation>{het.kip}</Pronunciation>
  <p class="my-2">華語：{@html het.definition}</p>
  <div class="italic text-sm">資料提供：{het.DataProvidedBy}</div>
{/each}
