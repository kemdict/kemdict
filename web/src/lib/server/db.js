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
 * Return heteronyms which match TITLE.
 * TODO: this should also treat exact pronunciation matches as title matches.
 * @param {string} title
 * @returns {object}
 */
export function getHeteronyms(title) {
  const stmt = db.prepare("SELECT * FROM heteronyms WHERE title = ?");
  let ret = stmt.all(title);
  if (ret) {
    ret = ret.map(processHet);
  }
  return ret;
}

export function getBacklinks(title) {
  const stmt = db.prepare(`SELECT DISTINCT "from" FROM links WHERE "to" = ?`);
  // Pluck mode: we get ["word", ...], and not [{"from": "word"}, ...]
  stmt.pluck(true);
  return stmt.all(title);
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
