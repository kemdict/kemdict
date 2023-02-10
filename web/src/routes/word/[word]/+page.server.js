export const prerender = false;

import { error } from "@sveltejs/kit";
import { getHeteronyms, getBacklinks } from "$lib/server/db.js";

/** @type {import('./$types').PageServerLoad} */
export function load({ params }) {
  const title = params.word;
  const heteronyms = getHeteronyms(title);
  const backlinks = getBacklinks(title);

  if (heteronyms) {
    return { title, heteronyms, backlinks };
  } else {
    throw error(404, "`Word ${w} not found.`");
  }
}
