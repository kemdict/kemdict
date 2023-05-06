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
 * | title | pns   | from         | props                      |
 * | word  | [...] | hakkadict    | {"definition": "...", ...} |
 * | word  | [...] | dict_revised | {"definition": "...", ...} |
 *
 * @name heteronyms-to-sqlite.js
 */

import fs from "node:fs";
import readline from "node:readline";
import Database from "better-sqlite3";

if (!fs.existsSync("heteronyms.json")) {
  console.log("heteronyms.json should be generated first!");
  process.exit(1);
}
if (fs.existsSync("entries.db")) {
  fs.rmSync("entries.db");
}
const db = new Database("entries.db");

function stringifyFields(thing) {
  for (const key of ["props", "pns"]) {
    if (typeof thing[key] !== "string") {
      thing[key] = JSON.stringify(thing[key]);
    }
  }
  return thing;
}

// title: string
// from?: string
//   null = this should not be shown in results
// pns?: array
// props: object
db.exec(
  `
PRAGMA user_version = 2;

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
  "title" NOT NULL,
  "from" REFERENCES dicts("id"),
  "pns",
  "props" NOT NULL
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
  const langs = Object.entries({
    zh_TW: "華語",
    nan_TW: "台語",
    hak_TW: "客語",
    han: "漢字",
  });
  const dicts = [
    {
      id: "unihan",
      name: "Unihan 資料庫",
      url: "https://www.unicode.org/cgi-bin/GetUnihanData.pl?codepoint=$1",
      lang: "han",
    },
    {
      id: "chhoetaigi_taioanpehoekichhoogiku",
      name: "台灣白話基礎語句",
      url: "https://chhoe.taigi.info/TaioanPehoeKichhooGiku/$1",
      lang: "nan_TW",
    },
    {
      id: "kisaragi_dict",
      name: "如月的現代台灣華語補足典",
      url: "/dict-kisaragi",
      lang: "zh_TW",
    },
    {
      id: "dict_concised",
      name: "國語辭典簡編本",
      url: "https://dict.concised.moe.edu.tw/search.jsp?word=$1",
      lang: "zh_TW",
    },
    {
      id: "dict_revised",
      name: "重編國語辭典修訂本",
      url: "https://dict.revised.moe.edu.tw/search.jsp?word=$1",
      lang: "zh_TW",
    },
    {
      id: "chhoetaigi_taijittoasutian",
      name: "台日大辭典台語譯本 (1932)",
      url: "https://taigi.fhl.net/dict/search.php?DETAIL=1&LIMIT=id=$1",
      lang: "nan_TW",
    },
    {
      id: "moedict_twblg",
      name: "臺灣閩南語常用詞辭典",
      url: "https://twblg.dict.edu.tw/holodict_new/result_main.jsp?radiobutton=1&limit=20&querytarget=1&sample=$1",
      lang: "nan_TW",
    },
    {
      id: "chhoetaigi_itaigi",
      name: "iTaigi 華台對照典",
      url: "https://itaigi.tw/k/$1",
      lang: "nan_TW",
    },
    {
      id: "hakkadict",
      name: "臺灣客家語常用詞辭典",
      url: 'https://hakkadict.moe.edu.tw/cgi-bin/gs32/gsweb.cgi?o=dalldb&s=id="$1".&searchmode=basic',
      lang: "hak_TW",
    },
    {
      id: "dict_idioms",
      name: "成語典",
      url: "https://dict.idioms.moe.edu.tw/idiomList.jsp?idiom=$1",
      lang: "zh_TW",
    },
  ];
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
  EachPT(langs, "Preparing langs: ", ([id, name]) => {
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
  heteronyms ("title","from","pns","props")
VALUES
  (@title,@from,@pns,@props)`);
  EachPT(heteronyms, "Inserting heteronyms into DB: ", (het) => {
    insertHet.run(stringifyFields(het));
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
  cast(json_tree.value as integer) AS stroke_count
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'stroke_count'
  AND length(title) = 1;

CREATE TABLE c AS
SELECT DISTINCT
  title,
  cast(json_tree.value as integer) AS non_radical_stroke_count
FROM heteronyms, json_tree(heteronyms.props)
WHERE json_tree.key = 'non_radical_stroke_count'
  AND length(title) = 1;

CREATE TABLE han AS
SELECT DISTINCT
  heteronyms.title,
  a.radical,
  b.stroke_count,
  c.non_radical_stroke_count
FROM heteronyms
LEFT JOIN a ON a.title = heteronyms.title
LEFT JOIN b ON b.title = heteronyms.title
LEFT JOIN c ON c.title = heteronyms.title
WHERE length(heteronyms.title) = 1
  AND a.radical                  IS NOT NULL
  AND b.stroke_count             IS NOT NULL
  AND c.non_radical_stroke_count IS NOT NULL
ORDER BY b.stroke_count;

DROP TABLE a;
DROP TABLE b;
DROP TABLE c;
`
);
