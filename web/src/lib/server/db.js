import * as fs from "node:fs";
import * as zlib from "node:zlib";
import Database from "better-sqlite3";
// This already uses ES6 sets when available.
import uniq from "lodash-es/uniq";

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
  let path = [
    "./src/lib/entries.db",
    "./src/lib/entries.db.gz",
    // If we build on Netlify, like the path copying doesn't work well
    // with the project being in a subdirectory, ie. during build we get
    // src/... but during serverless function runtime we get web/src/...
    //
    // Just, like, work around that.
    "./web/src/lib/entries.db.gz",
  ].find((f) => fs.existsSync(f));
  return readDB(path);
})();

/**
 * Return the word object for `title` from the DB, or undefined if it
 * doesn't exist.
 * @param {string} title
 * @returns {object}
 */
export function getWord(title) {
  const stmt = db.prepare("SELECT * FROM entries WHERE title = ?");
  let ret = stmt.get(title);
  if (ret) {
    return processWord(ret);
  } else {
    return ret;
  }
}

export function getBacklinks(title) {
  const stmt = db.prepare(`SELECT "from" FROM links WHERE "to" = ?`);
  // Pluck mode: we get ["word", ...], and not [{"from": "word"}, ...]
  stmt.pluck(true);
  return uniq(stmt.all(title));
}

/**
 * Process a word object so that all its props are JS objects and not
 * JSON strings.
 * @param {object} word
 * @returns {object}
 */
export function processWord(word) {
  for (let prop in word) {
    if (prop !== "title") {
      word[prop] = JSON.parse(word[prop]);
    }
  }
  return word;
}

/**
 * Get titles matching `needle` under `mode`. `mode` can only be
 * "prefix", "suffix", or "infix".
 * @param {string} mode
 * @param {string} needle
 * @returns {string[]}
 */
export function getTitles(mode, needle) {
  const stmt = db.prepare(`SELECT title FROM entries WHERE title LIKE ?`);
  if (mode === "prefix") {
    return stmt.all(`${needle}%`);
  } else if (mode === "suffix") {
    return stmt.all(`%${needle}`);
  } else if (mode === "infix") {
    return stmt.all(`%${needle}%`);
  } else {
    // TODO: error management
  }
}
