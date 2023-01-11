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
  "chhoetaigi_itaigi",
  "chhoetaigi_taijittoasutian",
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
  ${dicts.join(",")})`
).run();

db.prepare(
  `
CREATE TABLE pronunciations (
  title NOT NULL,
  pronunciation NOT NULL)`
).run();

db.prepare(
  `
CREATE TABLE links (
  "from" NOT NULL,
  "to" NOT NULL)`
).run();

/**
 * Run `func` for each element of `array`, with a progress display, in
 * a transaction.
 * @param {array} array - The array to iterate over.
 * @param {string} message - The message for the progress display.
 * @param {function} func - Function called for each element.
 argument, the element.
 */
const EachPT = db.transaction((array, message = "", func) => {
  // Whether we should print progress.
  const verbose =
    // Never verbose in CI; never verbose in Emacs except when in vterm
    !process.env.CI &&
    !(process.env.INSIDE_EMACS && !process.env.INSIDE_EMACS.includes("vterm"));

  // Iterate through each entry, with optional verbose output.
  let i = 0;
  const length = array.length;
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
      process.stdout.write(
        message + `${i + 1} / ${length} (${progress}%, ${diff}/s)`
      );
    }
    func(array[i]);
  }
  process.stdout.write("\n");
});

{
  const entries = Object.values(JSON.parse(fs.readFileSync("combined.json")));
  const insertEntry = db.prepare(`
INSERT INTO
  entries (title,${dicts.join(",")})
VALUES
  (@title,${dicts.map((x) => `@${x}`).join(",")})`);
  const insertPronunciation = db.prepare(`
INSERT INTO
  pronunciations (title,pronunciation)
VALUES
  (?, ?)`);
  EachPT(entries, "Inserting entries into DB: ", (entry) => {
    insertEntry.run(stringifyFields(entry));
    if (entry.pronunciations) {
      for (const pronunciation of entry.pronunciations) {
        insertPronunciation.run(entry.title, pronunciation);
      }
    }
  });
}

{
  const links = Object.values(JSON.parse(fs.readFileSync("links.json")));
  const insertLink = db.prepare(`
INSERT INTO
  links ("from","to")
VALUES
  (@from,@to)`);
  EachPT(links, "Inserting links into DB: ", (entry) => {
    insertLink.run(entry);
  });
}
