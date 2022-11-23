export const prerender = false;

import { redirect } from "@sveltejs/kit";
import { db, processWord } from "$lib/server/db.js";
import { dicts, WordSort } from "$lib/common";

export function load({ url }) {
  const query = url.searchParams.get("q");
  const stmt = db.prepare(`SELECT * FROM entries WHERE title LIKE ?`);
  let words = stmt.all(`${query}%`);
  // Redirect on the only exact match
  if (words && words.length === 1 && words[0]?.title === query) {
    throw redirect(301, encodeURI(`/word/${words[0].title}`));
  } else {
    let sort = url.searchParams.get("s");
    let sortFn;
    if (sort === "desc") {
      sortFn = WordSort.descend;
    } else {
      sort = "asc";
      sortFn = WordSort.ascend;
    }
    if (typeof query !== "string") {
      throw redirect(301, "/");
    }
    words = words.map(processWord);
    words.sort(sortFn);
    let count = 0;
    for (const word of words) {
      for (const dict of Object.keys(dicts)) {
        if (word[dict]?.heteronyms) {
          count += word[dict].heteronyms.length;
        }
      }
    }
    return { sort: sort, query: query, words: words, count: count };
  }
}
