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
    const stmtPn = db.prepare(
      `
SELECT DISTINCT title
FROM pronunciations
WHERE pronunciation LIKE ?
LIMIT 150`
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
      `SELECT * FROM entries WHERE title = @title`
    );
    // HACK: this is very, *very*, slow, when there are lots of titles.
    // It is still slow even when I tried to do it in one SQL query.
    // So this can probably only be fixed by switching to a
    // one-definition-per-row structure in the database.
    wordsPn = db.transaction(() => {
      return titlesPn.map((title) => titleWordStmt.get(title));
    })();
  }

  // This stops the query from going into the /word/ page when redirecting
  url.searchParams.delete("q");
  // Redirect on the only exact match
  if (words && words.length === 1 && words[0]?.title === query) {
    throw redirect(301, encodeURI(`/word/${words[0].title}`));
  }

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
        // Eww.
        word[dict].heteronyms = word[dict].heteronyms.filter(
          (het) =>
            het?.pronunciation?.includes(query) ||
            het?.trs?.includes(query) ||
            het?.bopomofo?.includes(query) ||
            het?.pinyin?.includes(query)
        );
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
    morePn: wordsPn.length >= 150,
  };
}
