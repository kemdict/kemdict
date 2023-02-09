/**
 * Turn heteronyms.json into a database.
 *
 * Entries in heteronyms.json look like this if they were written in
 * YAML:
 *
 *     - title: word
 *       pronunciations: [...]
 *       from: dict_revised
 *       props:
 *       - ...
 *     - title: word
 *       pronunciations: [...]
 *       from: hakkadict
 *       props:
 *       - ...
 *     ...
 *
 * In the database, this becomes
 *
 * | title | from         | props                      |
 * | word  | hakkadict    | {"definition": "...", ...} |
 * | word  | dict_revised | {"definition": "...", ...} |
 *
 * @name heteronyms-to-sqlite.js
 */

import fs from "node:fs";
import readline from "node:readline";

if (!fs.existsSync("heteronyms.json")) {
  console.log("heteronyms.json should be generated first!");
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
  if (typeof thing.props !== "string") {
    thing.props = JSON.stringify(thing.props);
  }
  return thing;
}

db.prepare(
  `
CREATE TABLE heteronyms (
  "title" NOT NULL,
  "from" NOT NULL,
  "props" NOT NULL)`
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
  const heteronyms = JSON.parse(fs.readFileSync("heteronyms.json"));
  const insertHet = db.prepare(`
INSERT INTO
  heteronyms ("title","from","props")
VALUES
  (@title,@from,@props)`);
  const insertPronunciation = db.prepare(`
INSERT INTO
  pronunciations (title,pronunciation)
VALUES
  (?, ?)`);
  EachPT(heteronyms, "Inserting heteronyms into DB: ", (het) => {
    insertHet.run(stringifyFields(het));
    if (het.pronunciations) {
      for (const pronunciation of het.pronunciations) {
        insertPronunciation.run(het.title, pronunciation);
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
