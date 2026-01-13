<script lang="ts">
  import type { Heteronym } from "common";
  export let heteronyms: Heteronym[];
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
  import { taigiTitle } from "$src/server/db";
</script>

{#each heteronyms as het}
  <h1>{taigiTitle(het)}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  <Property key="華語" value={het.props.def} html={true}></Property>
  <Property key="資料提供" value={het.props.DataProvidedBy}></Property>
{/each}
