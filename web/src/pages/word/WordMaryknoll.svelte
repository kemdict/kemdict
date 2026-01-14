<script>
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  <Property key="其他" value={het.props.kipOthers} />
  {#if het.props.zh}
    <p>
      <Property key="華語" value={het.props.zh} html={true} />
    </p>
  {/if}
  <p class="def">
    <Property key="英語" value={het.props.en} />
  </p>
{/each}
