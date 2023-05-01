import * as fs from "node:fs";
import * as zlib from "node:zlib";
import Database from "better-sqlite3";
import { escape as sqlEscape } from "sqlstring";
import { uniq, chunk, sortBy } from "lodash-es";
import { groupByProp, WordSortFns, dicts, dictsByLang } from "$src/common";
import type { Heteronym } from "$src/common";

// This already uses ES6 sets when available.

/**
 * Read a gzipped SQLite database and return a Database
 * object for it as an in-memory database.
 * If `path` does not end in ".gz", try to return a connection instead
 * (without using an in-memory database).
 */
function readDB(path: string): Database {
  if (path.endsWith(".db.gz")) {
    const data = fs.readFileSync(path);
    const decompressed = zlib.gunzipSync(data);
    return new Database(decompressed);
  } else {
    return new Database(path, { readonly: true, fileMustExist: true });
  }
}

const db = (() => {
  const path = ["../kemdict.db", "./entries.db", "../../dicts/entries.db"].find(
    (f) => fs.existsSync(f)
  );
  if (path) {
    return readDB(path);
  } else {
    return;
  }
})();

/**
 * Return heteronyms which match QUERY in its title or pronunciations.
 *
 * If MTCH is:
 * - "prefix": match heteronyms starting with QUERY
 * - "suffix": match heteronyms ending with QUERY
 * - "contains": match heteronyms that contain QUERY
 * - any thing else (including "exact"): match heteronyms exactly
 *
 * Returns [matchingDicts, Heteronyms]
 */
export function getHeteronyms(
  query: string,
  options?: {
    mtch?: string;
    dicts?: string[];
  }
): [string[] | undefined, Heteronym[]] {
  const mtch = options?.mtch;
  const dicts = options?.dicts;
  const hasDicts = dicts && dicts.length > 0;
  const opt = {
    q: (() => {
      if (mtch === "prefix") {
        return `${query}%`;
      } else if (mtch === "suffix") {
        return `%${query}`;
      } else if (mtch === "contains") {
        return `%${query}%`;
      } else {
        return query;
      }
    })(),
  };
  const operator = mtch ? "LIKE" : "=";
  const matchingDictsStmt = db.prepare(`
SELECT DISTINCT "from"
FROM heteronyms, json_each(heteronyms.pns)
WHERE "from" IS NOT NULL
AND (title ${operator} @q OR json_each.value ${operator} @q)
`);
  matchingDictsStmt.pluck(true);
  const heteronymsStmt = db.prepare(
    `
SELECT DISTINCT heteronyms.*
FROM heteronyms, json_each(heteronyms.pns)
WHERE "from" IS NOT NULL
AND (title ${operator} @q OR json_each.value ${operator} @q)
${hasDicts ? `AND "from" IN (${sqlEscape(dicts)})` : ""}
`
  );
  const hets = heteronymsStmt.all(opt);
  let matchingDicts = undefined;
  if (hasDicts) {
    matchingDicts = matchingDictsStmt.all(opt);
  }
  return [matchingDicts, hets?.map(processHet)];
}

export function getBacklinks(...titles: string[]): string[] {
  const stmt = db.prepare(
    `
SELECT DISTINCT "from" FROM links
WHERE "to" IN (${sqlEscape(titles)})`
  );
  // Pluck mode: we get ["word", ...], and not [{"from": "word"}, ...]
  stmt.pluck(true);
  return stmt.all();
}

export function getDictTitles(from: string, limit?: number) {
  if (limit) {
    const stmt = db.prepare(
      `SELECT DISTINCT title FROM heteronyms WHERE "from" = ? LIMIT ?`
    );
    stmt.pluck(true);
    return stmt.all(from, limit);
  } else {
    const stmt = db.prepare(
      `SELECT DISTINCT title FROM heteronyms WHERE "from" = ?`
    );
    stmt.pluck(true);
    return stmt.all(from);
  }
}

function getChars(): {
  with_stroke: Array<{
    title: string;
    stroke_count: number;
  }>;
  without_stroke: Array<string>;
} {
  const strokeStmt = db.prepare(`
  SELECT DISTINCT
    heteronyms.title,
    group_concat("from") as dicts,
    cast(json_tree.value as integer) AS 'stroke_count'
  FROM heteronyms, json_tree(heteronyms.props)
  WHERE length("title") = 1
    AND json_tree.key = 'stroke_count'
  GROUP BY title
    HAVING dicts != 'unihan'
  ORDER BY 'stroke_count'
`);
  const nostrokeStmt = db.prepare(`
SELECT initial FROM (
  SELECT DISTINCT
    substr(heteronyms.title, 0, 2) AS 'initial',
    group_concat("from") as dicts
  FROM heteronyms
)
WHERE dicts != 'unihan'
`);
  const pnStmt = db.prepare(`
  SELECT DISTINCT
    substr(json_each.value, 0, 2) AS 'pnInitial'
  FROM heteronyms, json_each(heteronyms.pns)
  WHERE "pnInitial" IS NOT NULL
`);
  nostrokeStmt.pluck(true);
  pnStmt.pluck(true);
  const with_stroke: Array<{ title: string; stroke_count: number }> =
    strokeStmt.all();
  const s = new Set(with_stroke.map((x) => x.title));
  const without_stroke: string[] = uniq([
    ...nostrokeStmt.all(),
    ...pnStmt.all(),
  ])
    .filter((x: string) => !s.has(x))
    .sort();
  return { with_stroke, without_stroke };
}

export const chars = (() => {
  const { with_stroke, without_stroke } = getChars();
  const with_stroke_grouped = chunk(
    sortBy(with_stroke, "stroke_count"),
    1200
  ).map((page) => groupByProp(page, "stroke_count"));
  return { without_stroke, with_stroke_grouped };
})();

export function getCharsByRadical(radical: string) {
  const stmt = db.prepare(`
  SELECT DISTINCT
    title,
    non_radical_stroke_count
  FROM han
  WHERE radical = ?
  ORDER BY non_radical_stroke_count
`);

  return stmt.all(radical);
}

export function getRadicals() {
  return db
    .prepare(
      `
    SELECT DISTINCT radical, stroke_count
    FROM han
    WHERE non_radical_stroke_count = 0
    ORDER BY radical
`
    )
    .all();
}

/**
 * Decode the JSON in the props field in a heteronym object.
 */
export function processHet(het: Heteronym): Heteronym {
  if (typeof het.props === "string") {
    het.props = JSON.parse(het.props);
  }
  return het;
}

/**
 * Like search/index.astro's load() function.
 */
export function getHetFromUrl(
  url: URL,
  lang?: string
): [true, { heteronyms: Heteronym[]; mtch: string; query: string }];
export function getHetFromUrl(url: URL, lang?: string): [false, string] {
  const query: string | undefined = url.searchParams.get("q")?.trim();
  const mtch: string = url.searchParams.get("m") || "prefix";
  const sort: string = url.searchParams.get("s") || "asc";
  if (typeof query !== "string" || query.length === 0) {
    return [false, "/"];
  }
  const [matchingDicts, heteronyms] = getHeteronyms(query, {
    mtch,
    dicts: lang && dictsByLang[lang],
  });
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
  const langSet = new Set();
  for (const dictId of matchingDicts) {
    for (const dict of dicts) {
      const dictPresent = dictId === dict.id;
      if (dictPresent) {
        langSet.add(dict.lang);
      }
    }
  }
  return [true, { heteronyms, mtch, query, langSet }];
}
