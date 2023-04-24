<script lang="ts">
  import type { Heteronym } from "$src/common";
  import Out from "$src/components/Out.svelte";
  function asHex(char: string) {
    return char.codePointAt(0).toString(16);
  }
  export let heteronyms: Heteronym[];
  const keys = [
    { key: "pinyin", name: "拼音" },
    { key: "radical", name: "部首" },
    { key: "cangjie", name: "倉頡" },
  ];
</script>

<!-- TODO: sementic variant and other variants -->
<div class="my-2">
  {#each heteronyms as het}
    <p>
      <span>漢字：</span><Out
        after={false}
        title="在 GlyphWiki 上查看"
        href="https://glyphwiki.org/wiki/u{asHex(het.title)}">{het.title}</Out
      >
    </p>
    {#each keys as { key, name }}
      {#if het.props[key]}
        <p><span>{name}：</span>{het.props[key]}</p>
      {/if}
    {/each}
    <p>
      <span>筆畫：</span>共{het.props.stroke_count}畫，部首外共{het.props
        .non_radical_stroke_count}畫
    </p>
    {#if het.props?.defs?.length == 1}
      <div><span>定義：</span>{het.props.defs[0]}</div>
    {:else if het.props?.defs?.length > 1}
      <div class="flex">
        <span>定義：</span>
        <ul>
          {#each het.props.defs as def}
            <li class="before:opacity-70 before:content-['-_']">{def}</li>
          {/each}
        </ul>
      </div>
    {/if}
  {/each}
</div>

<style lang="postcss">
  span {
    @apply font-bold text-gray-400;
  }
</style>
