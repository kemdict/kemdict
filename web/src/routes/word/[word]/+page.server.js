export const prerender = false;

import { error } from "@sveltejs/kit";
import { getHeteronyms, getBacklinks } from "$lib/server/db.js";
import uniq from "lodash-es/uniq";

/** @type {import('./$types').PageServerLoad} */
export function load({ params }) {
  const title = params.word;
  const heteronyms = getHeteronyms(title);
  const titles = uniq(heteronyms.map((het) => het.title));
  const backlinks = getBacklinks(...titles).sort();

  if (heteronyms) {
    return { title, heteronyms, backlinks };
  } else {
    throw error(404, "`Word ${w} not found.`");
  }
}
