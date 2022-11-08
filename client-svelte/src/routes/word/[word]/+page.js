import { error } from "@sveltejs/kit";

export const prerender = false;

/** @type {import('./$types').PageLoad} */
export async function load({ params }) {
  // This uses /word/<word>.
  //
  // If we want /word/?word=<word>:
  //
  // - make this function receive `url` instead
  // - put this file and +page.svelte in word/ instead of word/[word]/
  // - read url.searchParams.get("word") instead of params.word
  //
  // And remember to update links pointing here elsewhere.
  const w = params.word;
  const titles = await fetch("/titles.json").then((r) => r.json());
  const combined = await fetch("/combined.json").then((r) => r.json());
  if (titles.includes(w)) {
    return { word: combined[w] };
  }
  throw error(404, "Not found");
}
