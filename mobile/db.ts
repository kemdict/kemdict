import * as FS from "expo-file-system";
import * as SQLite from "expo-sqlite";
import { Asset } from "expo-asset";
import type { Heteronym } from "common";
// import { escape as sqlEscape } from "sqlstring";
import {
  groupByProp,
  WordSortFns,
  dictsByLang,
  dictIdLang,
  dictIdsToLangs,
  tokenToQuery,
} from "common";

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

export async function getHeteronyms(
  tokens: string | string[],
  options?: {
    mtch?: string;
    dicts?: string[];
  }
): [string[] | undefined, Heteronym[]] {
  const db = await readDB();
  if (typeof tokens === "string") {
    tokens = [tokens];
  }
  const mtch = options?.mtch;
  const dicts = options?.dicts;
  const hasDicts = dicts && dicts.length > 0;
  const operator = mtch ? "LIKE" : "=";
  const hets = await new Promise((resolve) => {
    db.transaction((tx) => {
      tx.executeSql(
        `
SELECT DISTINCT heteronyms.*
FROM heteronyms, json_each(heteronyms.pns)
WHERE "from" IS NOT NULL
${tokens
  .map(() => `AND (title ${operator} ? OR json_each.value ${operator} ?)`)
  .join("\n")}`,
        (() => {
          const arr = [];
          const tokenCount = tokens.length;
          tokens.forEach((token, index) => {
            let query = tokenToQuery(
              token,
              mtch,
              index === 0,
              index === tokenCount - 1
            );
            arr.push(query);
            arr.push(query);
          });
          return arr;
        })(),
        (_, res) => resolve(res.rows._array)
      );
    });
  });
  let applicableHets = hets;
  if (hasDicts) {
    applicableHets = hets.filter((het) => dicts.includes(het.from));
  }
  // Across all hets, not just filtered
  const dictSet = new Set();
  const dictCountObj: Record<string, number> = {};
  for (const het of hets || []) {
    dictSet.add(het.from);
    dictCountObj[het.from] = (dictCountObj[het.from] || 0) + 1;
  }
  const matchingDicts = [...dictSet];
  return [matchingDicts, applicableHets?.map(processHet), dictCountObj];
}
