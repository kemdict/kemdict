<script>
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
</script>

{#each heteronyms as het}
  <h1>{het.title}</h1>
  {#each het.props?.pn || [] as pn}
    <Pronunciation>{pn}</Pronunciation>
  {/each}
  {#if het.props?.tags}
    <Property key="標籤">
      {#each het.props.tags as tag}
        <!-- <a href={`https://taigiwords.taigitv.org.tw/zh_tw/dic?orbithashtag=${tag.title}`}>#{tag.title}</a> -->
        <a class="mr-1" href={`/search?q=%23${tag.title}&m=exact`}>#{tag.title}</a>
      {/each}
    </Property>
  {/if}
  <p class="def">
    {#if het.props.zh}{@html het.props.zh}{/if}
  </p>
{/each}
