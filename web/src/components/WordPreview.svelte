<script lang="ts">
  import ListLink from "./ListLink.svelte";
  import type { Heteronym } from "common";
  import type { OutputWord } from "$dicts/ministry-of-education/kautian";
  import { dictIdToDict } from "common";
  import { processPn, hetExactMatch } from "$src/server/db";
  import clsx from "clsx";
  export let heteronyms: Heteronym[];
  export let searchQuery: string;

  /** Return the preview text of `het`. */
  export function hetPreview(het: Heteronym) {
    function strip(html: string | undefined): string {
      // https://stackoverflow.com/a/822464/6927814
      // This doesn't have to be perfect. We're not handling untrusted
      // input either.
      return html?.replace(/<[^>]*>?/gm, "") || "";
    }
    return strip(
      het.props.def ||
        het.props.defs?.map((x: any) => x.def).join("") ||
        (het.from === "kautian" &&
          (het as Heteronym<OutputWord>).props?.heteronyms
            .map((het) => het.def)
            .join("")) ||
        het.props.example ||
        het.props.zh ||
        het.props.en ||
        het.props.scientificName,
    );
  }
</script>

{#each heteronyms as het}
  <ListLink href="/word/{het.title}?lang={het.lang}#{het.from}">
    <svelte:fragment slot="heading">
      {het.title}{processPn(het)}
    </svelte:fragment>
    <svelte:fragment slot="afterHeading">
      {#if hetExactMatch(het, searchQuery)}
        <div
          class={clsx(
            "absolute -top-2 right-2 inline-block",
            "bg-gradient-to-tl from-purple-500 to-indigo-500",
            "text-white",
            "rounded px-2 py-1 text-sm shadow-lg",
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
