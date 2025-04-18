<script lang="ts">
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function nameLinks(names: { poj: string; han: string }[]) {
    return names
      .map((name) => `<a href="/word/${name.han}">${name.poj}/${name.han}</a>`)
      .join("；");
  }
</script>

{#each heteronyms as het}
  <div class="flex w-full items-center">
    <h1>{het.title}</h1>
  </div>
  <Pronunciation>{het.props.poj}</Pronunciation>
  <p class="def">
    學名：{het.props.scientificName}<br />
    {#if het.props.otherNames?.taigi}其他台語名：{@html nameLinks(
        het.props.otherNames?.taigi,
      )}<br />{/if}
    {#if het.props.otherNames?.hakka}其他客語名：{@html nameLinks(
        het.props.otherNames?.hakka,
      )}<br />{/if}
  </p>
{/each}
