import { chunk, sortBy } from "lodash-es";
import { groupByProp, parseQueryToTokens, CrossDB } from "common";
import type { Heteronym, LangId, Mtch } from "common";

export async function readDB() {
  const fs = await import("node:fs");
  const Database = (await import("better-sqlite3")).default;
  const path = [
    "../kemdict.db",
    "./entries.db",
    "../dicts/entries.db",
    "../../dicts/entries.db",
  ].find((f) => fs.existsSync(f));
  if (!path) throw new Error("DB not found!");
  const db = new Database(path, {
    readonly: true,
    fileMustExist: true,
  });
  return db;
}

export const DB = new CrossDB("web", readDB);

export const chars = await (async () => {
  const { with_stroke, without_stroke } = await DB.getChars();
  const with_stroke_grouped = chunk(sortBy(with_stroke, "sc"), 1200).map(
    (page) => groupByProp(page, "sc")
  );
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
          originalQuery: string;
          langSet: Set<LangId>;
          langCountObj: Record<LangId, number>;
        }
      | string
    ) // when the first item is false, this is a string
  ]
> {
  /**
   * Query text as written in the URL
   */
  const originalQuery: string | undefined = url.searchParams.get("q")?.trim();
  /**
   * Unicode normalized query
   */
  const query = originalQuery?.normalize("NFC");
  const mtch: Mtch = url.searchParams.get("m") || "prefix";
  const sort: string = url.searchParams.get("s") || "desc";
  if (typeof query !== "string" || query.length === 0) {
    return [false, "/"];
  }
  const tokens = parseQueryToTokens(query);
  const { presentLangSet, heteronyms, langCountObj } = await DB.getHeteronyms(
    tokens,
    {
      mtch,
      langs: lang && [lang],
    }
  );
  // Redirect if all matched heteronyms belong to the same title
  if (
    mtch === "prefix" &&
    heteronyms &&
    heteronyms.length > 0 &&
    heteronyms.length < 10 &&
    heteronyms.every((x) => x.title === heteronyms[0].title)
  ) {
    return [
      false,
      encodeURI(
        heteronyms.length === 1
          ? `/word/${heteronyms[0].title}?lang=${heteronyms[0].lang}`
          : `/word/${heteronyms[0].title}`
      ),
    ];
  }
  if (sort === "desc") {
    // Negative -> a comes first
    // Positive -> b comes first
    // 0 -> keep
    heteronyms.sort((a: Heteronym, b: Heteronym) => {
      if (a.exact && a.title === query) return -1;
      if (b.exact && b.title === query) return 1;
      return a.title < b.title ? -1 : 1;
    });
  } else {
    heteronyms.sort((a: Heteronym, b: Heteronym) => {
      if (a.exact && a.title === query) return -1;
      if (b.exact && b.title === query) return 1;
      return a.title > b.title ? -1 : 1;
    });
  }
  return [
    true,
    {
      heteronyms,
      mtch,
      query,
      originalQuery,
      langSet: presentLangSet,
      langCountObj,
    },
  ];
}
