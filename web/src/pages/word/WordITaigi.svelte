<script lang="ts">
  import type { Heteronym } from "common";
  export let heteronyms: Heteronym[];
  import Pronunciation from "$src/components/Pronunciation.svelte";
  function getTitle(het: Heteronym): string {
    // title is set to kip in the dicts Makefile;
    // Show POJ in title if they're not equal.
    if (het.title === het.props.titlePoj) {
      return het.title;
    } else {
      return `${het.title} (${het.props.titlePoj})`;
    }
  }
</script>

{#each heteronyms as het}
  <h1>{getTitle(het)}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  <p class="my-2">華語：{@html het.props.def}</p>
  <div class="text-sm italic">資料提供：{het.props.DataProvidedBy}</div>
{/each}
