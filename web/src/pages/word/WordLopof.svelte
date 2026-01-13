<script lang="ts">
  const { heteronyms } = $props();
  import Pronunciation from "$src/components/Pronunciation.svelte";
  import Property from "$src/components/Property.svelte";

  function nameLinks(
    names: { poj: string; han: string }[] | undefined,
    lang: string,
  ) {
    return names
      ?.map(
        (name) =>
          `<a href="/word/${name.han}?lang=${lang}">${name.poj}/${name.han}</a>`,
      )
      .join("<br/>");
  }
</script>

{#each heteronyms as het}
  <div class="flex w-full items-center">
    <h1>{het.title}</h1>
  </div>
  <Pronunciation>{het.props.poj}</Pronunciation>
  <p class="def">
    <Property key="學名" value={het.props.scientificName} />
    <Property
      key="其他台語名"
      value={nameLinks(het.props.otherNames?.taigi, "nan_TW")}
      html={true}
    />
    <Property
      key="其他客語名"
      value={nameLinks(het.props.otherNames?.hakka, "hak_TW")}
      html={true}
    />
  </p>
{/each}
