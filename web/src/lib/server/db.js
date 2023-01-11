import * as fs from "node:fs";
import * as duckdb from "duckdb";

/**
 * Read an SQLite database into a DuckDB in-memory database.
 * Not tested with compressed files.
 */
async function readDuckDB(path) {
  let db = new duckdb.Database(":memory:");
  return new Promise((resolve) => {
    db.exec(
      `
INSTALL sqlite;
LOAD sqlite;
SET GLOBAL sqlite_all_varchar=true;
CALL sqlite_attach('${path}')`,
      () => {
        resolve(db);
      }
    );
  });
}

export const db = await (() => {
  const path = ["../kemdict.db", "./src/lib/entries.db"].find((f) =>
    fs.existsSync(f)
  );
  if (path) {
    return readDuckDB(path);
  } else {
    return;
  }
})();

/**
 * Return the word object for `title` from the DB, or undefined if it
 * doesn't exist.
 * @param {string} title
 * @returns {object}
 */
export async function getWord(title, conn = db) {
  return new Promise((resolve) => {
    conn.all("SELECT * FROM entries WHERE title = ?", title, (_err, ret) => {
      if (ret) {
        resolve(processWord(ret[0]));
      } else {
        resolve(ret[0]);
      }
    });
  });
}

export async function getBacklinks(title) {
  return new Promise((resolve) => {
    db.all(
      `SELECT DISTINCT "from" FROM links WHERE "to" = ?`,
      title,
      (_err, res) => {
        resolve(res.map((x) => x.from));
      }
    );
  });
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
export async function getTitles(mode, needle) {
  const conn = db.connect();
  const stmt = conn.prepare(`SELECT * FROM entries WHERE title LIKE ?`);

  let pattern;
  if (mode === "prefix") {
    pattern = `${needle}%`;
  } else if (mode === "suffix") {
    pattern = `%${needle}`;
  } else if (mode === "contains") {
    pattern = `%${needle}%`;
  }

  return new Promise((resolve) => {
    stmt.all(pattern, (_err, res) => {
      resolve(res);
    });
  });
}
