<script>
  export let entry;
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
  <div>讀音：{het.kip}</div>
  <div class="italic">資料提供：{het.DataProvidedBy}</div>
  <p class="mt-2">華語：{@html het.definition}</p>
{/each}
