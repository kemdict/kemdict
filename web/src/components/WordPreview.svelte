<script lang="ts">
  import ListLink from "./ListLink.svelte";
  import { spc } from "$src/processing";
  import { dictsObj } from "$src/common";
  import type { Heteronym } from "$src/common";
  export let heteronyms: Heteronym[];
  // FIXME: for Hakkadict, it's questionable for me to pick one
  // dialect out of the six provided.
  const pron_keys = [
    "bopomofo",
    "trs",
    "pronunciation",
    "p_四縣",
    "poj",
    "kip",
  ];
  function strip(html: string | undefined): string {
    // https://stackoverflow.com/a/822464/6927814
    // This doesn't have to be perfect. We're not handling untrusted
    // input either.
    return html?.replace(/<[^>]*>?/gm, "") || "";
  }
  function processPreview(def: string | undefined): string {
    return strip(def);
  }
  function processPn(het: Heteronym) {
    let pn: string | undefined =
      het.props[pron_keys.find((pron) => het.props[pron])];
    if (pn) {
      return `（${spc(pn)}）`;
    } else {
      return "";
    }
  }
</script>

{#each heteronyms as het}
  <ListLink href="/word/{het.title}?lang={dictsObj[het.from].lang}#{het.from}">
    <svelte:fragment slot="heading">
      {het.title}{processPn(het)}
    </svelte:fragment>
    <svelte:fragment slot="body">
      {processPreview(
        het.props.definition ||
          het.props.definitions?.map((x) => x.def).join("") ||
          het.props.example ||
          het.props.zh
      )}
    </svelte:fragment>
  </ListLink>
{/each}
