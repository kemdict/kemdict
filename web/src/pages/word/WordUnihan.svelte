<script lang="ts">
  import type { Heteronym } from "common";
  import Out from "$src/components/Out.svelte";
  import Property from "$src/components/Property.svelte";
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

<div class="my-2 space-y-2">
  {#each heteronyms as het}
    <div>
      <div class="my-0">
        <Property key="漢字">
          <Out
            after={false}
            title="在 GlyphWiki 上查看"
            href="https://glyphwiki.org/wiki/u{asHex(het.title)}"
            >{het.title}</Out
          >
        </Property>
      </div>
      {#each keys as { key, name }}
        <Property key={name} value={het.props[key]} />
      {/each}
      <div class="my-0">
        <Property key="筆畫"
          >共{het.props.sc}畫，部首外共{het.props.nrsc}畫</Property
        >
      </div>
      <Property key="簡體" value={het.props?.varS} html={true}></Property>
      <Property key="繁體" value={het.props?.varT} html={true}></Property>
      {#if het.props?.defs?.length == 1}
        <Property key="定義" value={het.props.defs[0]}></Property>
      {:else if het.props?.defs?.length > 1}
        <Property key="定義">
          <ul>
            {#each het.props.defs as def}
              <li class="before:opacity-70 before:content-['-_']">{def}</li>
            {/each}
          </ul>
        </Property>
      {/if}
    </div>
  {/each}
</div>
