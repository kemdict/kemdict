import * as fs from "node:fs";
import * as zlib from "node:zlib";
import Database from "better-sqlite3";

/**
 * Read a brotli-compressed SQLite database and return a Database
 * object for it as an in-memory database.
 * @param {string} path
 * @returns {Database}
 */
function readDB(path) {
  let data = fs.readFileSync(path);
  let decompressed = zlib.brotliDecompressSync(data);
  return new Database(decompressed);
}

export const db = (() => {
  // If we build on Netlify, like the path copying doesn't work well
  // with the project being in a subdirectory, ie. during build we get
  // src/... but during serverless function runtime we get web/src/...
  //
  // Just, like, work around that.
  try {
    return readDB("./src/lib/entries.db.br");
  } catch (e) {
    if (e instanceof Error && e.code === "ENOENT") {
      return readDB("./web/src/lib/entries.db.br");
    } else {
      throw e;
    }
  }
})();

/**
 * Return the word object for `title` from the DB, or undefined if it
 * doesn't exist.
 * @param {string} title
 * @returns {object}
 */
export function getWord(title) {
  const statement_word = db.prepare("SELECT * FROM entries WHERE title = ?");
  let ret = statement_word.get(title);
  if (ret) {
    return processWord(ret);
  } else {
    return ret;
  }
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
