/**
 * Turn heteronyms.json into a database.
 *
 * Entries in heteronyms.json look like this if they were written in
 * YAML:
 *
 *     - title: word
 *       pns: [...]
 *       from: dict_revised
 *       props:
 *       - ...
 *     - title: word
 *       pns: [...]
 *       from: hakkadict
 *       props:
 *       - ...
 *     ...
 *
 * In the database, this becomes
 *
 * dicts:
 * | id           | name | lang   |
 * | hakkadict    | ...  | hak_TW |
 * | dict_revised | ...  | zh_TW  |
 *
 * aliases ("false" and "true" are actually NULL and 1.0):
 * | alias | het_id | exact |
 * | abc   | 1      | false |
 * | def   | 1      | false |
 * | abc   | 2      | false |
 * | word  | 1      | true  |
 * | word  | 2      | true  |
 *
 * heteronyms:
 * | id | title | from         | props               |
 * | 1  | word  | hakkadict    | {"def": "...", ...} |
 * | 2  | word  | dict_revised | {"def": "...", ...} |
 *
 * @name heteronyms-to-sqlite.js
 */

import fs from "node:fs";
import readline from "node:readline";
import Database from "better-sqlite3";

import { langs, dicts } from "./data.mjs";
import { pnCollect, pnToInputForm, pnNormalize } from "./pn.mjs";

if (!fs.existsSync("heteronyms.json")) {
  console.log("heteronyms.json should be generated first!");
  process.exit(1);
}
if (fs.existsSync("entries.db")) {
  fs.rmSync("entries.db");
}
const db = new Database("entries.db");

function stringifyFields(thing) {
  for (const key of ["props"]) {
    if (typeof thing[key] !== "string") {
      thing[key] = JSON.stringify(thing[key]);
    }
  }
  return thing;
}

// title: string
// from?: string
//   null = this should not be shown in results
// props: object
db.exec(
  `
PRAGMA user_version = 4;

CREATE TABLE langs (
  "id" PRIMARY KEY,
  "name" NOT NULL
);

CREATE TABLE dicts (
  "id" PRIMARY KEY,
  "name" NOT NULL,
  "lang" REFERENCES langs("id")
);

CREATE TABLE heteronyms (
  "id" INTEGER PRIMARY KEY,
  "title" NOT NULL,
  "from" REFERENCES dicts("id"),
  "lang" REFERENCES langs("id"),
  "props" NOT NULL
);

CREATE TABLE aliases (
  "het_id" INTEGER REFERENCES heteronyms("id"),
  "alias" NOT NULL,
  "exact" INTEGER
);

CREATE TABLE links (
  "from" NOT NULL,
  "to" NOT NULL
);
`
);

/**
 * Run `func` for each element of `array`, with a progress display, in
 * a transaction.
 * @param {array} array - The array to iterate over.
 * @param {string} message - The message for the progress display.
 * @param {function} func - Function called for each element.
 * Receives two arguments, the element and the index.
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
    func(array[i], i);
  }
  process.stdout.write("\n");
});

{
  const langStmt = db.prepare(`
INSERT INTO
  langs ("id", "name")
VALUES
  (?, ?)`);
  const dictStmt = db.prepare(`
INSERT INTO
  dicts ("id", "name", "lang")
VALUES
  (@id,@name,@lang)`);
  EachPT(Object.entries(langs), "Preparing langs: ", ([id, name]) => {
    langStmt.run(id, name);
  });
  EachPT(dicts, "Preparing dicts: ", (dict) => {
    dictStmt.run(dict);
  });
}

{
  const heteronyms = JSON.parse(fs.readFileSync("heteronyms.json")).reverse();
  const insertHet = db.prepare(`
INSERT INTO
  heteronyms ("title","from","lang","props")
VALUES
  (@title,@from,@lang,@props)`);
  const insertAlias = db.prepare(`
INSERT INTO
  aliases ("het_id","alias","exact")
VALUES
  (@het_id,@alias,@exact)
`);
  EachPT(heteronyms, "Inserting heteronyms into DB: ", (het, i) => {
    insertHet.run(stringifyFields(het));
    // SQLite integer primary key is 1-based
    const het_id = i + 1;
    insertAlias.run({
      het_id,
      alias: het.title,
      exact: 1,
    });
    const pns = pnCollect(het).values();
    for (const pn of pns) {
      insertAlias.run({
        het_id,
        exact: 1,
        alias: pn,
      });
      // Input versions.
      // - don't duplicate if equal to original
      // - don't bother for some dictionaries
      if (
        [
          "moedict_twblg",
          "chhoetaigi_itaigi",
          "chhoetaigi_taioanpehoekichhoogiku",
          "chhoetaigi_taijittoasutian",
        ].includes(het.from)
      ) {
        const inputForm = pnToInputForm(pn);
        if (inputForm !== pn) {
          insertAlias.run({
            het_id,
            exact: null,
            alias: inputForm,
          });
        }
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

{
  console.log("Creating table 'han'...");
  db.exec(
    `
CREATE TABLE a AS
SELECT DISTINCT
  title,
  json_tree.value AS radical
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'radical'
  AND length(title) = 1;

CREATE TABLE b AS
SELECT DISTINCT
  title,
  cast(json_tree.value as integer) AS sc
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'sc'
  AND length(title) = 1;

CREATE TABLE c AS
SELECT DISTINCT
  title,
  cast(json_tree.value as integer) AS nrsc
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'nrsc'
  AND length(title) = 1;

CREATE TABLE han AS
SELECT DISTINCT
  heteronyms.title,
  a.radical,
  b.sc,
  c.nrsc
FROM heteronyms
LEFT JOIN a ON a.title = heteronyms.title
LEFT JOIN b ON b.title = heteronyms.title
LEFT JOIN c ON c.title = heteronyms.title
WHERE length(heteronyms.title) = 1
  AND a.radical    IS NOT NULL
  AND b.sc         IS NOT NULL
  AND c.nrsc       IS NOT NULL
ORDER BY b.sc;

DROP TABLE a;
DROP TABLE b;
DROP TABLE c;
VACUUM;
`
  );
}
