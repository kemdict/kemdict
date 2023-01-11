export const prerender = false;

import { redirect } from "@sveltejs/kit";
import { db, processWord, getTitles } from "$lib/server/db.js";
import { dictIds, WordSortFns } from "$lib/common";

export async function load({ url }) {
  const query = url.searchParams.get("q");
  const mtch = url.searchParams.get("m") || "prefix";
  const sort = url.searchParams.get("s") || "asc";
  let words = [];
  let wordsPn = [];

  if (typeof query !== "string") {
    throw redirect(301, "/");
  }

  words = await getTitles(mtch, query);

  {
    let titlesPn;
    let pattern;
    if (mtch === "prefix") {
      pattern = `${query}%`;
    } else if (mtch === "suffix") {
      pattern = `%${query}`;
    } else if (mtch === "contains") {
      pattern = `%${query}%`;
    }
    titlesPn = await new Promise((resolve) => {
      db.all(
        `
SELECT DISTINCT title
FROM pronunciations
WHERE pronunciation LIKE ?::STRING
LIMIT 100`,
        pattern,
        (_err, res) => {
          resolve(res);
        }
      );
    });
    titlesPn = titlesPn.map((x) => x.title);
    // let now = new Date();
    wordsPn = await new Promise((resolve) => {
      // This is a LOT faster than looping over titlesPn and using
      // proper quoting. Like, from 20+ seconds to less than a second
      // fast.
      //
      // It appears that using a parameter for a WHERE IN clause is,
      // just, not a thing in SQL. emacsql appears to allow you to
      // do it, but it's also just encoding ELisp vectors or strings into an
      // SQL string representation, like we're doing with titlesPn here.
      // (See `emacsql-escape-vector`.)
      db.all(
        `SELECT * FROM entries WHERE title IN (${titlesPn
          .map((x) => `'${x}'`)
          .join(",")})`,
        (_err, ret) => {
          resolve(ret || []);
        }
      );
    });
    // console.log(`q: ${query}`);
    // console.log(`took: ${(new Date() - now) / 1000}`);
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
    for (const dictId of dictIds) {
      if (word[dictId]?.heteronyms) {
        count += word[dictId].heteronyms.length;
      }
    }
  }
  for (const word of wordsPn) {
    for (const dictId of dictIds) {
      if (word[dictId]?.heteronyms) {
        // Eww.
        word[dictId].heteronyms = word[dictId].heteronyms.filter(
          (het) =>
            het?.pronunciation?.includes(query) ||
            het?.trs?.includes(query) ||
            het?.bopomofo?.includes(query) ||
            het?.pinyin?.includes(query)
        );
        countPn += word[dictId].heteronyms.length;
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
