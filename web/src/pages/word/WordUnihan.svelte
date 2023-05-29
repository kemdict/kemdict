<script lang="ts">
  import type { Heteronym } from "common";
  import Out from "$src/components/Out.svelte";
  function asHex(char: string) {
    return char.codePointAt(0).toString(16);
  }
  function ucsToString(ucs: string) {
    const hex = ucs.slice(2); // "U+1234" -> "1234"
    const codePoint = Number.parseInt(hex, 16);
    return String.fromCodePoint(codePoint);
  }
  function chars(characters: string[]) {
    return characters
      .map((c) => {
        const str = ucsToString(c);
        return `<a href="/word/${str}">${str}</a>`;
      })
      .join("、");
  }
  export let heteronyms: Heteronym[];
  const keys = [
    { key: "pinyin", name: "拼音" },
    { key: "radical", name: "部首" },
    { key: "cangjie", name: "倉頡" },
  ];
</script>

<!-- TODO: sementic variant and other variants -->
<div class="my-2 space-y-2">
  {#each heteronyms as het}
    <div>
      <div class="my-0">
        <span>漢字：</span><Out
          after={false}
          title="在 GlyphWiki 上查看"
          href="https://glyphwiki.org/wiki/u{asHex(het.title)}">{het.title}</Out
        >
      </div>
      {#each keys as { key, name }}
        {#if het.props[key]}
          <div class="my-0"><span>{name}：</span>{het.props[key]}</div>
        {/if}
      {/each}
      <div class="my-0">
        <span>筆畫：</span>共{het.props.sc}畫，部首外共{het.props.nrsc}畫
      </div>
      {#if het.props?.varS}
        <div><span>簡體：</span>{@html chars(het.props?.varS)}</div>
      {/if}
      {#if het.props?.varT}
        <div><span>繁體：</span>{@html chars(het.props?.varT)}</div>
      {/if}
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
    </div>
  {/each}
</div>

<style lang="postcss">
  span {
    @apply font-bold text-gray-400;
  }
</style>
