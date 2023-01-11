export const prerender = false;

import { error } from "@sveltejs/kit";
import { getWord, getBacklinks } from "$lib/server/db.js";

/** @type {import('./$types').PageServerLoad} */
export function load({ params }) {
  const w = params.word;
  const word = getWord(w);
  const backlinks = getBacklinks(w);

  if (word) {
    return { word: word, backlinks: backlinks };
  } else {
    throw error(404, "`Word ${w} not found.`");
  }
}
