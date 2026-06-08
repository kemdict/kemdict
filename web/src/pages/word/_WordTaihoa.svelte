<script lang="ts">
  import { sameBy } from "common";
  import type { Heteronym } from "common";
  import type { HetProps } from "$dicts/props/chhoetaigi_taihoa";
  interface Props {
    heteronyms: Heteronym<HetProps>[];
  }
  let { heteronyms }: Props = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
  import { taigiTitle } from "common";

  let safeToMerge = $derived(
    sameBy((het) => het.props.kip, heteronyms) &&
      sameBy((het) => het.props.kipInput, heteronyms) &&
      sameBy((het) => het.props.kipOthers, heteronyms) &&
      sameBy((het) => het.props.kipInputOthers, heteronyms),
  );
</script>

{#each heteronyms as het, i}
  {#if i === 0 || !safeToMerge}
    <h1>{taigiTitle(het)}</h1>
    <Pronunciation>{het.props.kip}</Pronunciation>
    {#if het.props.kipOthers}
      <Property key="其他講法">
        <Pronunciation>{het.props.kipOthers}</Pronunciation>
      </Property>
    {/if}
  {/if}
  {#if safeToMerge}
    {#if i === 0}
      <Property
        key="華語"
        value={heteronyms.map((het) => het.props.zh).join("、")}
        html={true}
      ></Property>
    {/if}
  {:else}
    <Property key="華語" value={het.props.zh} html={true}></Property>
  {/if}
{/each}
