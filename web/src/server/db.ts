import * as fs from "node:fs";
import * as zlib from "node:zlib";
import Database from "better-sqlite3";
import { uniq } from "lodash-es";
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

export function getChars(): {
  with_stroke: Array<{
    title: string;
    stroke_count: number;
  }>;
  without_stroke: Array<string>;
} {
  const strokeStmt = db.prepare(`
  SELECT DISTINCT
    heteronyms.title,
    cast(json_tree.value as integer) AS 'stroke_count'
  FROM heteronyms, json_tree(heteronyms.props)
  WHERE length("title") = 1
    AND json_tree.key = 'stroke_count'
  ORDER BY 'stroke_count'
`);
  const nostrokeStmt = db.prepare(`
  SELECT DISTINCT
    substr(heteronyms.title, 0, 2) AS 'initial'
  FROM heteronyms
`);
  const pnStmt = db.prepare(`
  SELECT DISTINCT
    substr(json_each.value, 0, 2) AS 'pnInitial'
  FROM heteronyms, json_each(heteronyms.pns)
  WHERE "pnInitial" IS NOT NULL
`);
  nostrokeStmt.pluck(true);
  pnStmt.pluck(true);
  const with_stroke = strokeStmt.all();
  let without_stroke: string[];
  let s = new Set(with_stroke.map((x) => x.title));
  without_stroke = uniq([...nostrokeStmt.all(), ...pnStmt.all()])
    .filter((x: any) => !s.has(x))
    .sort();
  return { with_stroke, without_stroke };
}

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
