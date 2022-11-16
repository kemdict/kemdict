/**
 * @file Accessors to the database.
 */

import * as fs from "node:fs";
import * as zlib from "node:zlib";
import Database from "better-sqlite3";

// Do this dance in order to not retain a reference to rawdb.
let db;
{
  let raw = fs.readFileSync("src/lib/entries.db.gz");
  let rawdb = zlib.gunzipSync(raw);
  db = new Database(rawdb);
}

const statement_word = db.prepare("select * from entries where title = ?");

/**
 * Return the word object from the DB.
 * @param {string} title
 * @returns {object}
 */
export function getWord(title) {
  // If the word doesn't exist it'll simply return `undefined`.
  let ret = statement_word.get(title);
  if (ret) {
    for (let prop in ret) {
      if (prop !== "title") {
        ret[prop] = JSON.parse(ret[prop]);
      }
    }
  }
  return ret;
}
