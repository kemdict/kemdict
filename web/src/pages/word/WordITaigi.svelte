<script lang="ts">
  import type { Heteronym } from "common";
  export let heteronyms: Heteronym[];
  import Pronunciation from "$src/components/Pronunciation.svelte";
  function getTitle(het: Heteronym): string {
    // title is from HanLoTaibunPoj
    if (het.title === het.props.HanLoTaibunKip) {
      return het.title;
    } else {
      return `${het.title} (${het.props.HanLoTaibunKip})`;
    }
  }
</script>

{#each heteronyms as het}
  <h1>{getTitle(het)}</h1>
  <Pronunciation>{het.props.poj}</Pronunciation>
  <p class="my-2">華語：{@html het.props.definition}</p>
  <div class="text-sm italic">資料提供：{het.props.DataProvidedBy}</div>
{/each}
