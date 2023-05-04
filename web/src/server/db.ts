import { uniq, chunk, sortBy } from "lodash-es";
import {
  groupByProp,
  WordSortFns,
  dictsByLang,
  dictIdLang,
  dictIdsToLangs,
  parseQueryToTokens,
} from "common";

import { getHeteronyms, getChars } from "common/db";
import type { Heteronym, LangId } from "common";

export const chars = await (async () => {
  const { with_stroke, without_stroke } = await getChars();
  const with_stroke_grouped = chunk(
    sortBy(with_stroke, "stroke_count"),
    1200
  ).map((page) => groupByProp(page, "stroke_count"));
  return { without_stroke, with_stroke_grouped };
})();

/**
 * Like search/index.astro's load() function.
 */
export async function getHetFromUrl(
  url: URL,
  lang?: string
): Promise<
  [
    boolean,
    (
      | {
          heteronyms: Heteronym[];
          mtch: string;
          query: string;
          langSet: Set<LangId>;
          langCountObj: Record<LangId, number>;
        }
      | string
    ) // when the first item is false, this is a string
  ]
> {
  const query: string | undefined = url.searchParams.get("q")?.trim();
  const mtch: string = url.searchParams.get("m") || "prefix";
  const sort: string = url.searchParams.get("s") || "asc";
  if (typeof query !== "string" || query.length === 0) {
    return [false, "/"];
  }
  const tokens = parseQueryToTokens(query);
  const [matchingDictIds, heteronyms, dictCountObj] = await getHeteronyms(
    tokens,
    {
      mtch,
      dicts: lang && dictsByLang[lang],
    }
  );
  // Redirect if all matched heteronyms belong to the same title
  if (
    heteronyms &&
    heteronyms.length > 0 &&
    heteronyms.length < 10 &&
    heteronyms.every((x) => x.title === heteronyms[0].title)
  ) {
    return [false, encodeURI(`/word/${heteronyms[0].title}`)];
  }
  let sortFn: typeof WordSortFns.descend;
  if (sort === "desc") {
    sortFn = WordSortFns.descend;
  } else {
    sortFn = WordSortFns.ascend;
  }
  heteronyms.sort(sortFn);
  const langSet = dictIdsToLangs(...matchingDictIds);
  const langCountObj: Record<LangId, number> = {};
  for (const [dictId, count] of Object.entries(dictCountObj)) {
    langCountObj[dictIdLang(dictId)] =
      (langCountObj[dictIdLang(dictId)] || 0) + count;
  }
  return [true, { heteronyms, mtch, query, langSet, langCountObj }];
}
