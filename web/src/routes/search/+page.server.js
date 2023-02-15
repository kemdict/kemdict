export const prerender = false;

import { redirect } from "@sveltejs/kit";
import { getHeteronyms } from "$lib/server/db.js";
import { dicts, langs, WordSortFns } from "$lib/common";

export function load({ url }) {
  const query = url.searchParams.get("q");
  const mtch = url.searchParams.get("m") || "prefix";
  const sort = url.searchParams.get("s") || "asc";
  let heteronyms = [];

  if (typeof query !== "string") {
    throw redirect(301, "/");
  }

  heteronyms = getHeteronyms(query, mtch);

  // This stops the query from going into the /word/ page when redirecting
  url.searchParams.delete("q");
  // Redirect if all matched heteronyms belong to the same title
  if (
    heteronyms &&
    heteronyms.length < 10 &&
    heteronyms.every((x) => x.title === heteronyms[0].title)
  ) {
    throw redirect(301, encodeURI(`/word/${heteronyms[0].props.title}`));
  }

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
