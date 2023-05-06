import { chunk, sortBy } from "lodash-es";
import {
  groupByProp,
  WordSortFns,
  dictsByLang,
  dictIdLang,
  dictIdsToLangs,
  parseQueryToTokens,
  CrossDB,
} from "common";
import type { Heteronym, LangId } from "common";

export async function readDB() {
  const fs = await import("node:fs");
  const zlib = await import("node:zlib");
  const Database = (await import("better-sqlite3")).default;
  const path = [
    "../kemdict.db",
    "./entries.db",
    "../dicts/entries.db",
    "../../dicts/entries.db",
  ].find((f) => fs.existsSync(f));
  if (!path) throw new Error("DB not found!");
  if (path.endsWith(".db.gz")) {
    const data = fs.readFileSync(path);
    const decompressed = zlib.gunzipSync(data);
    return new Database(decompressed);
  } else {
    return new Database(path, {
      readonly: true,
      fileMustExist: true,
    });
  }
}

export const DB = new CrossDB("web", readDB);

export const chars = await (async () => {
  const { with_stroke, without_stroke } = await DB.getChars();
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
  const [matchingDictIds, heteronyms, dictCountObj] = await DB.getHeteronyms(
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
