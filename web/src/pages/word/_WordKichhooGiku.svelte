<script>
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
  import { toTL } from "$src/hack";
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  <Property key="其他" value={het.props.kipOthers} />
  <p class="def">
    <Property key="華語" value={het.props.zh} html={true} />
    <Property key="英語" value={het.props.en} />
  </p>
  {#if het.props.examplePOJ || het.props.exampleEn || het.props.exampleZh}
    <blockquote>
      {#if het.props.examplePOJ}{@html toTL(het.props.examplePOJ)}<br />{/if}
      {#if het.props.exampleEn}{@html het.props.exampleEn}<br />{/if}
      {#if het.props.exampleZh}{@html het.props.exampleZh}{/if}
    </blockquote>
  {/if}
{/each}
