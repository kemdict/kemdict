/**
 * Turn combined.json into a database.
 *
 * An entry in combined.json looks something like this if it were
 * written in YAML:
 *
 *     - title: word
 *       pronunciations: [...]
 *       dict_revised:
 *         heteronyms: [...]
 *       hakkadict:
 *         id: ...
 *         heteronyms: [...]
 *       ...
 *
 * In the database, this becomes
 *
 * | title | dict_revised            | hakkadict            |
 * | word  | "{\"heteronyms\": ...}" | "{\"id\": ..., ...}" |
 *
 * @name combined-to-sqlite.js
 */

import fs from "node:fs";
import readline from "node:readline";

if (!fs.existsSync("combined.json")) {
  console.log("combined.json should be generated first!");
  process.exit(1);
}

const entries = Object.values(JSON.parse(fs.readFileSync("combined.json")));

if (fs.existsSync("entries.db")) {
  fs.rmSync("entries.db");
}

import Database from "better-sqlite3";
const db = new Database("entries.db");

const dicts = [
  "kisaragi_dict",
  "dict_revised",
  "hakkadict",
  "dict_concised",
  "dict_idioms",
  "moedict_twblg",
];

function stringifyFields(thing) {
  for (const dict of dicts) {
    if (typeof thing[dict] !== "string") {
      thing[dict] = JSON.stringify(thing[dict]);
    }
  }
  return thing;
}

db.prepare(
  `
CREATE TABLE entries (
  title NOT NULL,
  ${dicts.join(",")}
)`
).run();

db.prepare(
  `
CREATE TABLE pronunciations (
  title NOT NULL,
  pronunciation NOT NULL
)
`
).run();

const doInsert = db.transaction(() => {
  // Whether we should print progress.
  const verbose =
    // Never verbose in CI; never verbose in Emacs except when in vterm
    !process.env.CI &&
    !(process.env.INSIDE_EMACS && !process.env.INSIDE_EMACS.includes("vterm"));
  const insertEntry = db.prepare(`
  INSERT INTO
    entries (title,${dicts.join(",")})
    values (@title,${dicts.map((x) => `@${x}`).join(",")})`);
  const insertPronunciation = db.prepare(`
  INSERT INTO
    pronunciations (title,pronunciation)
    values (?, ?)`);

  let i = 0;
  const length = entries.length;
  let last = { time: new Date().getTime(), i: i };
  let diff = 0;
  for (i = 0; i < length; i++) {
    if (verbose) {
      let now = new Date().getTime();
      if (now - last.time > 1000) {
        diff = i - last.i;
        last.time = now;
        last.i = i;
      }
      let progress = Math.floor(((i + 1) / length) * 100);
      readline.cursorTo(process.stdout, 0);
      process.stdout.write(`${i + 1} / ${length} (${progress}%, ${diff}/s)`);
    }
    insertEntry.run(stringifyFields(entries[i]));
    if (entries[i].pronunciations) {
      for (const pronunciation of entries[i].pronunciations) {
        insertPronunciation.run(entries[i].title, pronunciation);
      }
    }
  }
  process.stdout.write("\n");
});

doInsert();
