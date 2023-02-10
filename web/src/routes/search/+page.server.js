export const prerender = false;

import { redirect } from "@sveltejs/kit";
import { db, processHet } from "$lib/server/db.js";
import { dicts, langs, WordSortFns } from "$lib/common";
import uniq from "lodash-es/uniq";

export function load({ url }) {
  const query = url.searchParams.get("q");
  const mtch = url.searchParams.get("m") || "prefix";
  const sort = url.searchParams.get("s") || "asc";
  let heteronyms = [];

  if (typeof query !== "string") {
    throw redirect(301, "/");
  }

  {
    let titles = [];
    const stmtHet = db.prepare(
      `SELECT DISTINCT title FROM heteronyms WHERE title LIKE ?`
    );
    const stmtPn = db.prepare(
      `
SELECT DISTINCT title
FROM pronunciations
WHERE pronunciation LIKE ?`
    );
    stmtHet.pluck(true);
    stmtPn.pluck(true);
    if (mtch === "prefix") {
      titles = stmtHet.all(`${query}%`);
    } else if (mode === "suffix") {
      titles = stmtHet.all(`%${query}`);
    } else if (mode === "contains") {
      titles = stmtHet.all(`%${query}%`);
    }
    if (mtch === "prefix") {
      titles = [...stmtPn.all(`${query}%`), ...titles];
    } else if (mtch === "suffix") {
      titles = [...stmtPn.all(`%${query}`), ...titles];
    } else if (mtch === "contains") {
      titles = [...stmtPn.all(`%${query}%`), ...titles];
    }
    const titleWordStmt = db.prepare(
      `SELECT * FROM heteronyms WHERE title IN (${uniq(titles)
        .map((x) => `'${x}'`)
        .join(",")})`
    );
    heteronyms = db.transaction(() => titleWordStmt.all())();
  }

  // This stops the query from going into the /word/ page when redirecting
  url.searchParams.delete("q");
  // Redirect on the only exact match
  if (
    heteronyms &&
    heteronyms.length === 1 &&
    heteronyms[0].props?.title === query
  ) {
    throw redirect(301, encodeURI(`/word/${heteronyms[0].props.title}`));
  }

  heteronyms = heteronyms.map(processHet);

  let sortFn;
  if (sort === "desc") {
    sortFn = WordSortFns.descend;
  } else {
    sortFn = WordSortFns.ascend;
  }
  heteronyms.sort(sortFn);

  let count = heteronyms.length;
  let langSet = new Set();
  for (const het of heteronyms) {
    for (const dict of dicts) {
      let dictPresent = het.from === dict.id;
      if (dictPresent) {
        langSet.add(dict.lang);
      }
    }
  }
  return {
    match: mtch,
    sort,
    query,
    heteronyms: heteronyms,
    count,
    langs: Object.entries(langs).filter((l) => langSet.has(l[0])),
  };
}
