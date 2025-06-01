<script>
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  {#if het.props.kipOthers}
    <div>其他：{het.props.kipOthers}</div>
  {/if}
  {#if het.props.zh}
    <p>
      華語：
      {@html het.props.zh}
    </p>
  {/if}
  <p class="def">
    {#if het.props.en}英語：{het.props.en}{/if}
  </p>
{/each}
