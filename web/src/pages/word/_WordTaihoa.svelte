<script lang="ts">
  import type { Heteronym } from "common";
  interface Props {
    heteronyms: Heteronym[];
  }
  let { heteronyms }: Props = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
  import { taigiTitle } from "common";

  // let allSame = heteronyms.map((het) => het.props.kip);
</script>

{#each heteronyms as het}
  <h1>{taigiTitle(het)}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  {#if het.props.kipOthers}
    <Property key="其他講法">
      <Pronunciation>{het.props.kipOthers}</Pronunciation>
    </Property>
  {/if}
  <Property key="華語" value={het.props.zh} html={true}></Property>
{/each}
