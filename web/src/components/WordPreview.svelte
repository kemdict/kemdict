<script lang="ts">
  import ListLink from "./ListLink.svelte";
  import { spc } from "$src/processing";
  import type { Heteronym } from "common";
  import clsx from "clsx";
  export let heteronyms: Heteronym[];
  export let searchQuery: string;
  // FIXME: for Hakkadict, it's questionable for me to pick one
  // dialect out of the six provided.
  const pron_keys = [
    "bopomofo",
    "trs",
    "pronunciation",
    "p_四縣",
    "kip",
    "poj",
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
    if (pn && het.title !== pn) {
      return `（${spc(pn)}）`;
    } else {
      return "";
    }
  }
</script>

{#each heteronyms as het}
  <ListLink href="/word/{het.title}?lang={het.lang}#{het.from}">
    <svelte:fragment slot="heading">
      {het.title}{processPn(het)}
    </svelte:fragment>
    <svelte:fragment slot="afterHeading">
      {#if het.exact && searchQuery && het.title === searchQuery}
        <div
          class={clsx(
            "absolute -top-2 right-2 inline-block",
            "bg-gradient-to-tl from-purple-500 to-indigo-500",
            "text-white",
            "rounded px-2 py-1 text-sm shadow-lg"
          )}
        >
          完全相符
        </div>
      {/if}
    </svelte:fragment>
    <svelte:fragment slot="body">
      {processPreview(
        het.props.def ||
          het.props.defs?.map((x) => x.def).join("") ||
          het.props.example ||
          het.props.zh
      )}
    </svelte:fragment>
  </ListLink>
{/each}
