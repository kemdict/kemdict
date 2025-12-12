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
    <div class="flex gap-2 flex-wrap">
      {#each het.props.tags as tag}
        <a href={`https://taigiwords.taigitv.org.tw/zh_tw/dic?orbithashtag=${tag.title}`}>#{tag.title}</a>
      {/each}
    </div>
  {/if}
  <p class="def">
    {#if het.props.zh}{het.props.zh}{/if}
  </p>
{/each}
