<script lang="ts">
  import ListLink from "./ListLink.svelte";
  import type { Heteronym } from "common";
  import type { OutputWord } from "$dicts/ministry-of-education/kautian";
  import { dictIdToDict } from "common";
  import { hetExactMatch } from "$src/server/db";
  import clsx from "clsx";
  export let heteronymsAndPn: [Heteronym, string | undefined][];
  export let searchQuery: string;

  function strip(html: string | undefined): string {
    // https://stackoverflow.com/a/822464/6927814
    // This doesn't have to be perfect. We're not handling untrusted
    // input either.
    return html?.replace(/<[^>]*>?/gm, "") || "";
  }

  /** Add parens onto `str` if it's not empty or undefined. */
  function parens(str: string | undefined) {
    return str ? `（${str}）` : undefined;
  }

  /** Return the preview text of `het`. */
  function hetPreview(het: Heteronym) {
    return strip(
      het.props.def ||
        het.props.defs?.map((x: any) => x.def).join("") ||
        (het.from === "kautian" &&
          (het as Heteronym<OutputWord>).props?.heteronyms
            ?.map((het) => het.def)
            .join("")) ||
        (het.from === "kautian" &&
          (het as Heteronym<OutputWord>).props?.type === "單字不成詞者" &&
          "（單字不成詞者，無義項）") ||
        ((het as Heteronym<OutputWord>).props?.type === "臺華共同詞" &&
          "（臺華共同詞，無義項）") ||
        het.props.example ||
        het.props.zh ||
        het.props.en ||
        het.props.scientificName,
    );
  }
</script>

{#each heteronymsAndPn as [het, pn]}
  <ListLink href="/word/{het.title}?lang={het.lang}#{het.from}">
    <svelte:fragment slot="heading">
      {het.title}{parens(pn)}
    </svelte:fragment>
    <svelte:fragment slot="afterHeading">
      {#if hetExactMatch(het, searchQuery)}
        <div
          class={clsx(
            "absolute -top-2 right-2 inline-block",
            "bg-indigo-700",
            "text-white",
            "rounded px-2 py-1 text-sm shadow-md",
          )}
        >
          完全相符
        </div>
      {/if}
    </svelte:fragment>
    <svelte:fragment slot="body">
      {hetPreview(het)}
    </svelte:fragment>
    <svelte:fragment slot="after">
      <div class="text-gray-500 dark:text-stone-300">
        {#if het.from}
          {dictIdToDict(het.from).name}
        {/if}
      </div>
    </svelte:fragment>
  </ListLink>
{/each}
