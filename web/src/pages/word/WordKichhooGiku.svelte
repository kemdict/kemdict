<script>
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import { toTL } from "$src/hack";
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  {#if het.props.kipOthers}
    <div>其他：{het.props.kipOthers}</div>
  {/if}
  <p class="def">
    {#if het.props.zh}華語：{@html het.props.zh}<br />{/if}
    {#if het.props.en}英語：{het.props.en}{/if}
  </p>
  {#if het.props.examplePOJ || het.props.exampleEn || het.props.exampleZh}
    <blockquote>
      {#if het.props.examplePOJ}{@html toTL(het.props.examplePOJ)}<br />{/if}
      {#if het.props.exampleEn}{@html het.props.exampleEn}<br />{/if}
      {#if het.props.exampleZh}{@html het.props.exampleZh}{/if}
    </blockquote>
  {/if}
{/each}
