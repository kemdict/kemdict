<script lang="ts">
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";

  function nameLinks(names: { poj: string; han: string }[], lang: string) {
    return names
      .map((name) => `<a href="/word/${name.han}?lang=${lang}">${name.poj}/${name.han}</a>`)
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
        het.props.otherNames?.taigi, "nan_TW"
      )}<br />{/if}
    {#if het.props.otherNames?.hakka}其他客語名：{@html nameLinks(
        het.props.otherNames?.hakka, "hak_TW"
      )}<br />{/if}
  </p>
{/each}
