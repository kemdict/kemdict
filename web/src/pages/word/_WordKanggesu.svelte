<script>
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  <Pronunciation>{het.props.kip}</Pronunciation>
  {#if het.props.mainTypeName}
    {@const tag = het.props.mainTypeName}
    <Property key="工藝類別">
      <a href={`/search?q=%23${tag}&m=exact`}>{tag}</a>
    </Property>
  {/if}
  {#if het.props.childTypeName}
    {@const tag = het.props.childTypeName}
    <Property key="子類別">
      <a href={`/search?q=%23${tag}&m=exact`}>{tag}</a>
    </Property>
  {/if}
  {#if het.props.zh}
    <Property key="華語" value={het.props.zh} html={true} />
  {/if}
  {#if het.props.memo}
    <p class="def">
      {@html het.props.memo}
    </p>
  {/if}
  {#if het.props.provider}
    <Property key="資料來源" value={het.props.provider} />
  {/if}
{/each}
