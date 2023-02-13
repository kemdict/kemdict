import * as fs from "node:fs";
import * as zlib from "node:zlib";
import Database from "better-sqlite3";
// This already uses ES6 sets when available.

/**
 * Read a gzipped SQLite database and return a Database
 * object for it as an in-memory database.
 * If `path` does not end in ".gz", try to return a connection instead
 * (without using an in-memory database).
 * @param {string} path
 * @returns {Database}
 */
function readDB(path) {
  if (path.endsWith(".db.gz")) {
    let data = fs.readFileSync(path);
    let decompressed = zlib.gunzipSync(data);
    return new Database(decompressed);
  } else {
    return new Database(path, { readonly: true, fileMustExist: true });
  }
}

export const db = (() => {
  const path = [
    "../kemdict.db",
    "./src/lib/entries.db",
    "./src/lib/entries.db.gz",
    // If we build on Netlify, like the path copying doesn't work well
    // with the project being in a subdirectory, ie. during build we get
    // src/... but during serverless function runtime we get web/src/...
    //
    // Just, like, work around that.
    "./web/src/lib/entries.db.gz",
  ].find((f) => fs.existsSync(f));
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
 * - any thing else: match heteronyms exactly
 *
 * @param {string} query
 * @param {string?} mtch
 * @returns {Array<object>}
 */
export function getHeteronyms(query, mtch) {
  let opt;
  let operator = "LIKE";
  if (mtch === "prefix") {
    opt = { q: `${query}%` };
  } else if (mtch === "suffix") {
    opt = { q: `%${query}` };
  } else if (mtch === "contains") {
    opt = { q: `%${query}%` };
  } else {
    opt = { q: query };
    operator = "=";
  }
  const stmt = db.prepare(
    `
SELECT DISTINCT heteronyms.*
FROM heteronyms, json_each(heteronyms.pns)
WHERE
  title ${operator} @q
OR
  json_each.value ${operator} @q`
  );
  return stmt.all(opt)?.map(processHet);
}

export function getBacklinks(...titles) {
  const stmt = db.prepare(
    `SELECT DISTINCT "from" FROM links WHERE "to" IN (${titles
      .map((x) => `'${x}'`)
      .join(",")})`
  );
  // Pluck mode: we get ["word", ...], and not [{"from": "word"}, ...]
  stmt.pluck(true);
  return stmt.all();
}

/**
 * Decode the JSON in the props field in a heteronym object.
 * @param {object} het
 * @returns {object}
 */
export function processHet(het) {
  if (typeof het.props === "string") {
    het.props = JSON.parse(het.props);
  }
  return het;
}
