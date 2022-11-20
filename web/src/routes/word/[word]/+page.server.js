export const prerender = false;

import { error } from "@sveltejs/kit";
import { getWord } from "$lib/server/db.js";

/** @type {import('./$types').PageServerLoad} */
export function load({ params }) {
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
  const word = getWord(w);

  if (word) {
    return { word: word };
  } else {
    throw error(404, "`Word ${w} not found.`");
  }
}
