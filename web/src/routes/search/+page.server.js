export const prerender = false;

import { redirect } from "@sveltejs/kit";
import { db, processWord } from "$lib/server/db.js";
import { dicts, WordSortFns } from "$lib/common";

export function load({ url }) {
  const query = url.searchParams.get("q");
  const mtch = url.searchParams.get("m") || "prefix";
  const sort = url.searchParams.get("s") || "asc";
  let words;
  let wordsPn;

  if (typeof query !== "string") {
    throw redirect(301, "/");
  }

  {
    const stmt = db.prepare(`SELECT * FROM entries WHERE title LIKE ?`);
    if (mtch === "prefix") {
      words = stmt.all(`${query}%`);
    } else if (mtch === "suffix") {
      words = stmt.all(`%${query}`);
    } else if (mtch === "contains") {
      words = stmt.all(`%${query}%`);
    }
  }

  {
    // FIXME: this is matching a JSON array as a string.
    // Pronunciations should be stored in another title -> pronunciation table.
    const stmtPn = db.prepare(
      `SELECT * FROM entries WHERE pronunciations LIKE ?`
    );
    // Because pronunciations is just a JSON string, we can only use
    // "contains". Otherwise this should also adhere to match type.
    wordsPn = stmtPn.all(`%${query}%`);
  }

  // This stops the query from going into the /word/ page when redirecting
  url.searchParams.delete("q");
  // Redirect on the only exact match
  if (words && words.length === 1 && words[0]?.title === query) {
    throw redirect(301, encodeURI(`/word/${words[0].title}`));
  }
  // if (words.length === 0) {
  //   return { query: query, words: words, count: 0 };
  // }

  words = words.map(processWord);
  wordsPn = wordsPn.map(processWord);

  let sortFn;
  if (sort === "desc") {
    sortFn = WordSortFns.descend;
  } else {
    sortFn = WordSortFns.ascend;
  }
  words.sort(sortFn);
  wordsPn.sort(sortFn);

  // FIXME: after match type works with pronunciations both should
  // be combined.
  let count = 0;
  let countPn = 0;
  for (const word of words) {
    for (const dict of Object.keys(dicts)) {
      if (word[dict]?.heteronyms) {
        count += word[dict].heteronyms.length;
      }
    }
  }
  for (const word of wordsPn) {
    for (const dict of Object.keys(dicts)) {
      if (word[dict]?.heteronyms) {
        countPn += word[dict].heteronyms.length;
      }
    }
  }
  return {
    match: mtch,
    sort: sort,
    query: query,
    words: words,
    count: count,
    wordsPn: wordsPn,
    countPn: countPn,
  };
}
