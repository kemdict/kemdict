import { error } from "@sveltejs/kit";
import combined from "$lib/combined.json";
import titles from "$lib/titles.json";

/** @type {import('./$types').PageLoad} */
export function load({ url }) {
  // This uses /word/?word=<word>, allowing for those pages to not
  // have to be prerendered.
  //
  // If we want /word/<word>:
  //
  // - make this function receive `params` instead
  // - put this file and +page.svelte in word/[word]/ instead of word/
  // - read params.word instead of url.searchParams.get("word")
  const w = url.searchParams.get("word");
  if (titles.includes(w)) {
    return { word: combined[w] };
  }
  throw error(404, "Not found");
}
