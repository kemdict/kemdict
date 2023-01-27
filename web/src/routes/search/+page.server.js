export const prerender = false;

import { redirect } from "@sveltejs/kit";
import { db, processWord, getTitles } from "$lib/server/db.js";
import { dicts, langs, WordSortFns } from "$lib/common";

export function load({ url }) {
  const query = url.searchParams.get("q");
  const mtch = url.searchParams.get("m") || "prefix";
  const sort = url.searchParams.get("s") || "asc";
  let words = [];

  if (typeof query !== "string") {
    throw redirect(301, "/");
  }

  words = getTitles(mtch, query);

  {
    const stmtPn = db.prepare(
      `
SELECT DISTINCT title
FROM pronunciations
WHERE pronunciation LIKE ?`
    );
    let titlesPn;
    if (mtch === "prefix") {
      titlesPn = stmtPn.all(`${query}%`);
    } else if (mtch === "suffix") {
      titlesPn = stmtPn.all(`%${query}`);
    } else if (mtch === "contains") {
      titlesPn = stmtPn.all(`%${query}%`);
    }
    const titleWordStmt = db.prepare(
      `SELECT * FROM entries WHERE title IN (${titlesPn
        .map((x) => `'${x.title}'`)
        .join(",")})`
    );
    // FIXME: words can be matched with both title and pronunciation
    // and thus appear twice.
    words = [...words, ...db.transaction(() => titleWordStmt.all())()];
  }

  // This stops the query from going into the /word/ page when redirecting
  url.searchParams.delete("q");
  // Redirect on the only exact match
  if (words && words.length === 1 && words[0]?.title === query) {
    throw redirect(301, encodeURI(`/word/${words[0].title}`));
  }

  words = words.map(processWord);

  let sortFn;
  if (sort === "desc") {
    sortFn = WordSortFns.descend;
  } else {
    sortFn = WordSortFns.ascend;
  }
  words.sort(sortFn);

  // FIXME: after match type works with pronunciations both should
  // be combined.
  let count = 0;
  let langSet = new Set();
  for (const word of words) {
    for (const dict of dicts) {
      let dictPresent = false;
      if (word[dict.id]?.heteronyms) {
        // Eww.
        word[dict.id].heteronyms = word[dict.id].heteronyms.filter(
          (het) =>
            het?.pronunciation?.includes(query) ||
            het?.trs?.includes(query) ||
            het?.bopomofo?.includes(query) ||
            het?.pinyin?.includes(query) ||
            het?.title?.includes(query)
        );
        let hets = word[dict.id].heteronyms.length;
        count += hets;
        if (hets !== 0) dictPresent = true;
      }
      if (dictPresent) {
        langSet.add(dict.lang);
      }
    }
  }
  return {
    match: mtch,
    sort: sort,
    query: query,
    words: words,
    count: count,
    langs: Object.entries(langs).filter((l) => langSet.has(l[0])),
  };
}
