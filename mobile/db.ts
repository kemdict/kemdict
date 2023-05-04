import * as FS from "expo-file-system";
import * as SQLite from "expo-sqlite";
import { Asset } from "expo-asset";

import { tokenToLIKEInput } from "./common/index.ts";

const runtime = "rn";

/**
 * Read the database.
 */
export async function readDB(): Promise<SQLite.WebSQLDatabase> {
  let info: FS.FileInfo;
  // TODO: verify integrity
  // info = await FS.getInfoAsync(FS.documentDirectory + "SQLite/entries.db");
  // if (info.exists) {
  //   await FS.deleteAsync(FS.documentDirectory + "SQLite/entries.db");
  // }
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite");
  if (!info.exists) {
    await FS.makeDirectoryAsync(FS.documentDirectory + "SQLite");
  }
  info = await FS.getInfoAsync(FS.documentDirectory + "SQLite/entries.db");
  if (!info.exists) {
    console.log("downloading");
    await FS.downloadAsync(
      // require only works with static values. (On Hermes only, I guess??)
      Asset.fromModule(require("./assets/entries.db")).uri,
      FS.documentDirectory + "SQLite/entries.db"
    );
    console.log("done");
  }
  return SQLite.openDatabase("entries.db");
}

/* --- code below this point is copied from common/db.ts. --- */

export async function crossDbAll(
  source: string,
  args: unknown[] = [],
  pluck?: boolean
): Promise<unknown[]> {
  if (runtime === "web") {
    const db = await readDB();
    const stmt = db.prepare(source);
    if (pluck) stmt.pluck(pluck);
    return stmt.all(...args);
  } else {
    const db = await readDB();
    return new Promise((resolve) => {
      db.transaction((tx) =>
        tx.executeSql(source, args as Array<string | number>, (_, resultSet) =>
          resolve(resultSet.rows._array)
        )
      );
    });
  }
}

/**
 * Return heteronyms which match every TOKENS in its title or pronunciations.
 *
 * If MTCH is:
 * - "prefix": match heteronyms starting with the first TOKEN
 * - "suffix": match heteronyms ending with the last TOKEN
 * - "contains": match heteronyms that contain TOKEN
 * - any thing else (including "exact"): match heteronyms exactly
 *
 * Returns [matchingDicts, Heteronyms]
 */
export async function getHeteronyms(
  tokens: string | string[],
  options?: {
    mtch?: string;
    dicts?: string[];
  }
): Promise<[string[] | undefined, Heteronym[], Record<DictId, number>]> {
  if (typeof tokens === "string") {
    tokens = [tokens];
  }
  const mtch = options?.mtch;
  const dicts = options?.dicts;
  const hasDicts = dicts && dicts.length > 0;
  const operator = mtch ? "LIKE" : "=";
  const hets = (await crossDbAll(
    // TODO: create another index table (normalized token, hetId) so
    // we don't have to parse arrays like this, and also to make it
    // easier to support search without tones
    `
SELECT DISTINCT heteronyms.*
FROM heteronyms, json_each(heteronyms.pns)
WHERE "from" IS NOT NULL
${tokens
  .map(() => `AND (title ${operator} ? OR json_each.value ${operator} ?)`)
  .join("\n")}
`,
    (() => {
      const arr = [];
      const tokenCount = tokens.length;
      tokens.forEach((token, index) => {
        const query = tokenToLIKEInput(
          token,
          mtch,
          index === 0,
          index === tokenCount - 1
        );
        arr.push(query);
        arr.push(query);
      });
      return arr;
    })()
  )) as Heteronym[];
  let applicableHets = hets;
  if (hasDicts) {
    applicableHets = hets.filter((het) => dicts.includes(het.from));
  }
  // Across all hets, not just filtered
  const dictSet: Set<DictId> = new Set();
  const dictCountObj: Record<string, number> = {};
  for (const het of hets || []) {
    dictSet.add(het.from);
    dictCountObj[het.from] = (dictCountObj[het.from] || 0) + 1;
  }
  const matchingDicts = [...dictSet];
  return [matchingDicts, applicableHets?.map(processHet), dictCountObj];
}

export async function getBacklinks(...titles: string[]): Promise<string[]> {
  return (await crossDbAll(
    `
SELECT DISTINCT "from" FROM links
WHERE "to" IN (${sqlEscape(titles)})`,
    [],
    true
  )) as string[];
}

export async function getDictTitles(
  from: string,
  limit?: number
): Promise<string[]> {
  if (limit) {
    return (await crossDbAll(
      `SELECT DISTINCT title FROM heteronyms WHERE "from" = ? LIMIT ?`,
      [from, limit],
      true
    )) as string[];
  } else {
    return (await crossDbAll(
      `SELECT DISTINCT title FROM heteronyms WHERE "from" = ?`,
      [from],
      true
    )) as string[];
  }
}

export async function getChars(): Promise<{
  with_stroke: Array<{
    title: string;
    stroke_count: number;
  }>;
  without_stroke: Array<string>;
}> {
  const with_stroke = (await crossDbAll(
    `
  SELECT DISTINCT
    heteronyms.title,
    group_concat("from") as dicts,
    cast(json_tree.value as integer) AS 'stroke_count'
  FROM heteronyms, json_tree(heteronyms.props)
  WHERE length("title") = 1
    AND json_tree.key = 'stroke_count'
  GROUP BY title
    HAVING dicts != 'unihan'
  ORDER BY 'stroke_count';
`
  )) as Array<{
    title: string;
    stroke_count: number;
  }>;
  const nostroke = (await crossDbAll(
    `
SELECT initial FROM (
  SELECT DISTINCT
    substr(heteronyms.title, 0, 2) AS 'initial',
    group_concat("from") as dicts
  FROM heteronyms
)
WHERE dicts != 'unihan'`,
    [],
    true
  )) as string[];
  const pn = (await crossDbAll(
    `
  SELECT DISTINCT
    substr(json_each.value, 0, 2) AS 'pnInitial'
  FROM heteronyms, json_each(heteronyms.pns)
  WHERE "pnInitial" IS NOT NULL
`,
    [],
    true
  )) as string[];
  const s = new Set(with_stroke.map((x) => x.title));
  const without_stroke: string[] = uniq([...nostroke, ...pn])
    .filter((x: string) => !s.has(x))
    .sort();
  return { with_stroke, without_stroke };
}

export async function getCharsByRadical(radical: string) {
  return await crossDbAll(
    `
  SELECT DISTINCT
    title,
    non_radical_stroke_count
  FROM han
  WHERE radical = ?
  ORDER BY non_radical_stroke_count
`,
    [radical]
  );
}

export async function getRadicals() {
  return await crossDbAll(
    `
    SELECT DISTINCT radical, stroke_count
    FROM han
    WHERE non_radical_stroke_count = 0
    ORDER BY radical
`
  );
}
