<script lang="ts">
  import type { Heteronym } from "common";
  export let heteronyms: Heteronym[];
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { taigiTitle } from "$src/server/db";
</script>

{#each heteronyms as het}
  <h1>{taigiTitle(het)}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  <p class="my-2">華語：{@html het.props.def}</p>
  <div class="text-sm italic">資料提供：{het.props.DataProvidedBy}</div>
{/each}
