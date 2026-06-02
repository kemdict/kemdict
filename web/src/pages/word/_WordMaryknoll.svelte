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
    <div>
      <Property key="華語" value={het.props.zh} html={true} />
    </div>
  {/if}
  <div class="def">
    <Property key="英語" value={het.props.en} />
  </div>
{/each}
