import * as fs from "node:fs";
import * as zlib from "node:zlib";
import Database from "better-sqlite3";
import { uniq, chunk } from "lodash-es";
import type { Heteronym } from "$src/common";
import { groupByProp } from "$src/common";

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
 * @returns {Array<object>}
 */
export function getHeteronyms(
  query: string,
  mtch?: string | undefined
): Heteronym[] {
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
  const stmt = db.prepare(
    `
SELECT DISTINCT heteronyms.*
FROM heteronyms, json_each(heteronyms.pns)
WHERE "from" IS NOT NULL
  AND (title ${operator} @q OR json_each.value ${operator} @q)`
  );
  return stmt.all(opt)?.map(processHet);
}

export function getBacklinks(...titles: string[]): string[] {
  const stmt = db.prepare(
    `SELECT DISTINCT "from" FROM links WHERE "to" IN (${titles
      .map((x) => `'${x}'`)
      .join(",")})`
  );
  // Pluck mode: we get ["word", ...], and not [{"from": "word"}, ...]
  stmt.pluck(true);
  return stmt.all();
}

export function getDictTitles(from: string, limit?: number | undefined) {
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

/** Paginate a result from groupByProp.
 * Try to keep each page under `size`.
 */
function paginate<T>(
  arr: Array<[any, T[]]>,
  size = 500
): Array<Array<[any, T[]]>> {
  const res = [];
  let buffer = 0;
  let i = 0;
  function push(group: [any, T[]]) {
    if (!res[i]) {
      res[i] = [];
    }
    res[i].push(group);
    buffer += group[1].length;
  }
  function _nextPage() {
    i += 1;
    buffer = 0;
  }
  for (const group of arr) {
    if (buffer + group[1].length > size) {
      _nextPage();
    }
    chunk(group[1], size).forEach((part, index) => {
      if (index > 0) {
        _nextPage();
      }
      push([group[0], part]);
    });
  }
  return res;
}

export const chars = (() => {
  const { with_stroke, without_stroke } = getChars();
  const with_stroke_grouped = paginate(
    groupByProp(with_stroke, "stroke_count"),
    1000
  );
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
